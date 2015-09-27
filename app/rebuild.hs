module Main where

import           Control.Monad.Logger (runStdoutLoggingT, filterLogger)
import           Data.Time (getCurrentTime)
import qualified Database.Persist
import           Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize)
import           Helpers.Heroku (herokuConf)
import           Import hiding (runDB)
import           System.Environment (lookupEnv)
import           Vote (reprocessVotes, serializeVotes, uploadDump, storeDump)
import           Yesod.Default.Config

main :: IO ()
main = do
  conf <- Yesod.Default.Config.loadConfig (configSettings Production) { csParseExtra = parseExtra }
  dbconf <- if development
           then withYamlEnvironment "config/postgresql.yml" (appEnv conf)
                Database.Persist.loadConfig >>=
                Database.Persist.applyEnv
           else herokuConf
  pool <- runStdoutLoggingT . filterLogger (\_ LevelDebug -> False)
         $ createPostgresqlPool (pgConnStr dbconf) (pgPoolSize dbconf)
  let runDB t = runStdoutLoggingT $ Database.Persist.runPool dbconf t pool
  (items, votes) <- runDB reprocessVotes
  bucket <- lookupEnv "ISAACRANKS_STATIC_BUCKET_NAME"
  case bucket of
    Just _ -> do
      (bucketName, name) <- uploadDump (serializeVotes items votes)
      timestamp <- getCurrentTime
      runDB $ storeDump bucketName name timestamp
    Nothing -> return ()
