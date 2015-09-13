module Main where

import           Application (makeFoundation)
import           Control.Monad.Logger (runLoggingT)
import           Control.Monad.Logger (runStdoutLoggingT)
import qualified Database.Persist
import           Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize)
import           Helpers.Heroku (herokuConf)
import           Import
import           Vote (reprocessVotes)
import           Yesod.Default.Config

main :: IO ()
main = do
  conf <- Yesod.Default.Config.loadConfig (configSettings Production) { csParseExtra = parseExtra }
  dbconf <- if development
           then withYamlEnvironment "config/postgresql.yml" (appEnv conf)
                Database.Persist.loadConfig >>=
                Database.Persist.applyEnv
           else herokuConf
  pool <- runStdoutLoggingT
         $ createPostgresqlPool (pgConnStr dbconf) (pgPoolSize dbconf)
  runStdoutLoggingT $ Database.Persist.runPool dbconf reprocessVotes pool
