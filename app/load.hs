module Main where

import Application (makeFoundation, getAppSettings)
import Control.Monad.Logger (runLoggingT)
import Database.Persist.Postgresql (runSqlPool)
import Import hiding (getArgs)
import Items (loadData)
import System.Environment (getArgs)

main :: IO ()
main = do
  [version, itemsFile, poolsFile] <- getArgs
  settings <- getAppSettings
  foundation <- makeFoundation settings
  let logFunc = messageLoggerSource foundation (appLogger foundation)
  runLoggingT (runSqlPool (loadData version itemsFile poolsFile) (appConnPool foundation)) logFunc
