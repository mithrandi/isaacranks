module Main where

import Application (makeFoundation, getAppSettings)
import Control.Monad.Logger (runLoggingT)
import Database.Persist.Postgresql (runSqlPool)
import Import hiding (getArgs)
import Vote (reprocessVotes, serializeVotes, uploadDump, storeDump)

main :: IO ()
main = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  let logFunc = messageLoggerSource foundation (appLogger foundation)
  runResourceT (runLoggingT (runSqlPool reprocessVotes (appConnPool foundation)) logFunc)
  -- (items, votes) <- runDB reprocessVotes
  -- bucket <- lookupEnv "ISAACRANKS_STATIC_BUCKET_NAME"
  -- case bucket of
  --   Just _ -> do
  --     (bucketName, name) <- uploadDump (serializeVotes items votes)
  --     timestamp <- getCurrentTime
  --     runDB $ storeDump bucketName name timestamp
  --   Nothing -> return ()
