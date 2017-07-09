{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , rebuildMain
    , develMain
    , makeFoundation
    , getAppSettings
    ) where

import           Control.Monad.Logger (liftLoc, runLoggingT, logInfo)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import           Data.Default (def)
import           Data.Text.Read (decimal)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Database.Persist.Postgresql
  ( createPostgresqlPool, pgConnStr, pgPoolSize, runSqlPool)
import           Database.Persist.Sql (runMigration)
import           Import
import           Instrument (requestDuration, instrumentApp, timeAction)
import           Language.Haskell.TH.Syntax (qLocation)
import           Network.HTTP.Client.Conduit (newManager)
import           Network.Wai (Middleware)
import           Network.Wai.Handler.Warp
  ( Settings, defaultSettings, defaultShouldDisplayException, runSettings
  , setHost, setOnException, setPort
  )
import           Network.Wai.Middleware.Autohead (autohead)
import           Network.Wai.Middleware.Gzip (gzip, gzipFiles, GzipFiles(GzipCompress))
import           Network.Wai.Middleware.Prometheus (metricsApp)
import           Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import           Prometheus
import           Prometheus.Metric.GHC (ghcMetrics)
import           System.Environment (getEnv)
import           System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize, toLogStr)
import qualified Web.ClientSession as WS

import           Vote (reprocessVotes)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import           Handler.Common
import           Handler.Home
import           Handler.Vote
import           Handler.Ranks
import           Handler.Donate
import           Handler.Changes

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

myMiddlewares :: Middleware
myMiddlewares = autohead . gzip def {gzipFiles = GzipCompress}

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    let appStatic = myStatic

    appBallotKey <- do
        Right key64 <- B64.decode . BC.pack <$> getEnv "BALLOT_MASKING_KEY"
        let Right key = WS.initKey key64
        return key

    appMetrics <- do
      metricBallots <- registerIO $ vector ("version" :: String) $
        histogram (Info "isaacranks_ballot_generation_seconds" "Ballot generation time in seconds.") defaultBuckets
      metricVotes <- registerIO $ vector ("version" :: String) $
        histogram (Info "isaacranks_vote_casting_seconds" "Vote casting time in seconds.") defaultBuckets
      metricLastRebuild <- registerIO $
        gauge (Info "isaacranks_last_rebuild_timestamp" "Timestamp of last ranks rebuild.")
      metricRebuildDuration <- registerIO $
        gauge (Info "isaacranks_last_rebuild_duration_seconds" "Duration of last ranks rebuild in seconds.")
      return AppMetrics {..}

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Return the foundation
    return $ mkFoundation pool

makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare . myMiddlewares $ appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation = do
  requests <- Prometheus.registerIO requestDuration
  void $ Prometheus.register ghcMetrics
  logger <- mkRequestLogger def
    { outputFormat =
        if appDetailedRequestLogging $ appSettings foundation
        then Detailed True
        else Apache
             (if appIpFromHeader $ appSettings foundation
               then FromFallback
               else FromSocket)
    , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
    }
  let instrument = instrumentApp requests "isaacranks"
  return $ logger . instrument


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [] [configSettingsYmlValue] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


rebuildMain :: IO ()
rebuildMain = do
  [intervalS] <- getArgs
  let interval = either (const 0) fst (decimal intervalS)
  settings <- getAppSettings
  foundation <- makeFoundation settings
  let logFunc = messageLoggerSource foundation (appLogger foundation)
      run d = runResourceT (runLoggingT (runSqlPool d (appConnPool foundation)) logFunc)
      reprocess' = run $
        reprocess (metricLastRebuild . appMetrics $ foundation)
                  (metricRebuildDuration . appMetrics $ foundation)
  void $ Prometheus.register ghcMetrics
  void $ fork $ runSettings (warpSettings foundation) metricsApp
  if interval == 0
    then reprocess'
    else forever $ threadDelay (interval * 1000000) >> reprocess'
  where reprocess lr rd = do
          $(logInfo) "Starting rebuild…"
          (_, duration) <- timeAction reprocessVotes
          now <- liftIO getPOSIXTime
          liftIO $ setGauge (fromRational . toRational $ now) lr
          liftIO $ setGauge duration rd
          $(logInfo) "Rebuild complete."

  -- (items, votes) <- runDB reprocessVotes
  -- bucket <- lookupEnv "ISAACRANKS_STATIC_BUCKET_NAME"
  -- case bucket of
  --   Just _ -> do
  --     (bucketName, name) <- uploadDump (serializeVotes items votes)
  --     timestamp <- getCurrentTime
  --     runDB $ storeDump bucketName name timestamp
  --   Nothing -> return ()
