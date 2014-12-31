module Main where

import           Application (makeFoundation)
import           Control.Monad.Logger (runLoggingT)
import           Control.Monad.Logger (runStdoutLoggingT)
import qualified Database.Persist
import           Import
import           Vote (reprocessVotes)
import           Yesod.Default.Config

main :: IO ()
main = do
  conf <- Yesod.Default.Config.loadConfig (configSettings Production) { csParseExtra = parseExtra }
  foundation <- makeFoundation conf
  runStdoutLoggingT $ Database.Persist.runPool (persistConfig foundation) reprocessVotes (connPool foundation)
