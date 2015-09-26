module Main where

import           Application (makeFoundation)
import           Control.Monad.Logger (runStdoutLoggingT, filterLogger)
import qualified Database.Persist
import           Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize)
import           Helpers.Heroku (herokuConf)
import           Import
import           Items (loadData)
import           System.Environment (getArgs)
import           Yesod.Default.Config

main :: IO ()
main = do
  [version, dataFile] <- getArgs
  conf <- Yesod.Default.Config.loadConfig (configSettings Production) { csParseExtra = parseExtra }
  dbconf <- if development
           then withYamlEnvironment "config/postgresql.yml" (appEnv conf)
                Database.Persist.loadConfig >>=
                Database.Persist.applyEnv
           else herokuConf
  pool <- runStdoutLoggingT . filterLogger (\_ LevelDebug -> False)
         $ createPostgresqlPool (pgConnStr dbconf) (pgPoolSize dbconf)
  runStdoutLoggingT $ Database.Persist.runPool dbconf (loadData version dataFile) pool
