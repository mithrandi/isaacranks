{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import           Control.Monad.Logger (runLoggingT)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import           Data.Default (def)
import qualified Database.Persist
import           Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize)
import           Database.Persist.Sql (runMigration)
import           Helpers.Heroku (herokuConf)
import           Import
import           Network.HTTP.Client.Conduit (newManager)
import           Network.Wai (Middleware)
import           Network.Wai.Logger (clockDateCacher)
import           Network.Wai.Middleware.AcceptOverride (acceptOverride)
import           Network.Wai.Middleware.Autohead (autohead)
import           Network.Wai.Middleware.Gzip (gzip, gzipFiles, GzipFiles(GzipCompress))
import           Network.Wai.Middleware.MethodOverride (methodOverride)
import           Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import           System.Environment (getEnv)
import           System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import qualified Web.ClientSession as WS
import           Yesod.Core.Types (loggerSet, Logger (Logger))
import           Yesod.Default.Config
import           Yesod.Default.Handlers
import           Yesod.Default.Main

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import           Handler.Home
import           Handler.Vote
import           Handler.Ranks
import           Handler.Donate

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

myMiddlewares :: Middleware
myMiddlewares = acceptOverride
                . autohead
                . gzip def {gzipFiles = GzipCompress}
                . methodOverride

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (logWare $ myMiddlewares app, logFunc)


-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager
    s <- staticSite
    dbconf <- if development
             then withYamlEnvironment "config/postgresql.yml" (appEnv conf)
                  Database.Persist.loadConfig >>=
                  Database.Persist.applyEnv
             else herokuConf

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    key <- do
        Right key64 <- (B64.decode . BC.pack <$> getEnv "BALLOT_MASKING_KEY")
        let Right key = WS.initKey key64
        return key

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        mkFoundation p = App
            { settings = conf
            , getStatic = s
            , connPool = p
            , httpManager = manager
            , persistConfig = dbconf
            , appLogger = logger
            , ballotKey = key
            }
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation logger

    p <- flip runLoggingT logFunc
       $ createPostgresqlPool (pgConnStr dbconf) (pgPoolSize dbconf)
    let foundation = mkFoundation p

    -- Perform database migration using our application's logging settings.
    flip runLoggingT logFunc
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
