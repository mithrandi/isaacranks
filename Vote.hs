module Vote where

import           Codec.Compression.GZip (compressWith, defaultCompressParams, bestCompression, CompressParams(compressLevel))
import           Control.Lens
import           Control.Monad (forM_)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson (encode)
import           Data.Aeson.Lens (_JSON, _Object, _Array)
import           Data.Binary.Get (runGet, getWord32be)
import           Data.Binary.Put (runPut, putWord32be)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.ByteString.Lazy as LBS
import           Data.List (foldl')
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import           Data.Time (getCurrentTime, UTCTime, NominalDiffTime, addUTCTime)
import           Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           Database.Persist.Sql (SqlBackend)
import           Import
import           Model.IsaacVersion
import           Network.AWS
import           Network.AWS.S3
import           System.Environment (lookupEnv)
import qualified Web.ClientSession as WS

k :: Double
k = 24.0

adjustment :: Double -> Double -> Double
adjustment s e = k * (s - e)

expected :: Double -> Double -> Double
expected r1 r2 = 1 / (1 + 10 ** ((r2 - r1) / 400))

processVote :: MonadIO m => IsaacVersion -> Int -> Int -> UTCTime -> Text -> Text -> ReaderT SqlBackend m ()
processVote ver w l timestamp voter rawBallot = do
  Just (Entity w' _) <- getBy $ UniqueItem ver w
  Just (Entity l' _) <- getBy $ UniqueItem ver l
  _ <- insert $ Ballot rawBallot timestamp
  _ <- insert $ Vote ver w' l' timestamp voter
  return ()

applyVote :: M.Map (Key Item) Item -> Vote -> M.Map (Key Item) Item
applyVote items vote =
  adjustRating (adjustment 1.0 (expected wr lr)) w .
  adjustRating (adjustment 0.0 (expected lr wr)) l $ items
  where w = vote^.voteWinner
        l = vote^.voteLoser
        (Just winner) = items^.at w
        (Just loser) = items^.at l
        wr = winner^.itemRating
        lr = loser^.itemRating
        adjustRating a = M.adjust
                         (\i -> i & itemRating +~ a & itemVotes +~ 1)

reprocessVotes ::
  (MonadIO m) => ReaderT SqlBackend m (M.Map (Key Item) Item, [Vote])
reprocessVotes = do
  updateWhere [] [ItemRating =. 500.1, ItemVotes =. 0]
  items <- M.fromList . map (\(Entity a b) -> (a, b)) <$> selectList [] []
  votes <- map entityVal <$> selectList [] [Asc VoteTimestamp]
  forM_ (M.toList $ foldl' applyVote items votes) $
    \(itemId, Item {_itemRating = r, _itemVotes = v}) -> update itemId [ItemRating =. r, ItemVotes =. v]
  now <- liftIO getCurrentTime
  deleteWhere [BallotTimestamp <=. (-validTime) `addUTCTime` now]
  return (items, votes)

serializeVotes :: M.Map (Key Item) Item -> [Vote] -> Value
serializeVotes items votes = _Array._Wrapped # map asJson votes
  where asJson vote = _Object._Wrapped #
                      [ ("version", vote^.voteVersion.re _JSON)
                      , ("winner", items^?!ix (vote^.voteWinner).itemIsaacId.re _JSON)
                      , ("loser", items^?!ix (vote^.voteLoser).itemIsaacId.re _JSON)
                      , ("timestamp", vote^.voteTimestamp.re _JSON)
                      ]

gzJson :: ToJSON a => a -> LBS.ByteString
gzJson = compressWith defaultCompressParams { compressLevel = bestCompression }
         . encode

staticEnv :: IO Network.AWS.Env
staticEnv = newEnv NorthVirginia (FromEnv "ISAACRANKS_AWS_ACCESS_KEY_ID" "ISAACRANKS_AWS_SECRET_ACCESS_KEY" Nothing)

staticConfig :: IO (Text, Text)
staticConfig = do
  e <- T.pack
      <$> fromMaybe "testing"
      <$> lookupEnv "YESOD_ENVIRONMENT"
  b <- T.pack
      <$> fromMaybe "static.isaacranks.com"
      <$> lookupEnv "ISAACRANKS_STATIC_BUCKET_NAME"
  return (e, b)

uploadDump :: Value -> IO (Text, Text)
uploadDump votes = do
  (envName, bucketName) <- staticConfig
  timestamp <- truncate <$> getPOSIXTime
  let filename = T.pack (show (timestamp :: Integer)) <> ".json"
      name = "data/" <> envName <> "/votes/" <> filename
      gzBody = toBody (gzJson votes)
  e <- staticEnv
  _ <- runResourceT . runAWS e . send $
      putObject (BucketName bucketName) (ObjectKey name) gzBody
      & poContentType ?~ "application/json"
      & poContentEncoding ?~ "gzip"
      & poContentDisposition ?~ "attachment; filename=" <> filename
      & poCacheControl ?~ "max-age=2147483647"
  TI.putStrLn $ "Uploaded votes to " <> bucketName <> "/" <> name
  return (bucketName, name)

storeDump :: Text -> Text -> UTCTime -> (MonadIO m) => ReaderT SqlBackend m ()
storeDump bucketName name timestamp = do
  _ <- insert $ Dump bucketName name timestamp
  return ()

encodeBallot :: UTCTime -> Int -> Int -> B.ByteString
encodeBallot expiry winner loser = toStrict . runPut $ do
  putWord32be (truncate . utcTimeToPOSIXSeconds $ expiry)
  putWord32be (fromIntegral winner)
  putWord32be (fromIntegral loser)

decodeBallot :: B.ByteString -> (UTCTime, Int, Int)
decodeBallot = go . fromStrict
  where go = runGet $ do
          expiry <- getWord32be
          winner <- getWord32be
          loser <- getWord32be
          return ( posixSecondsToUTCTime (fromIntegral expiry)
                 , fromIntegral winner
                 , fromIntegral loser)

-- 1 hour
validTime :: NominalDiffTime
validTime = 3600

encryptBallot :: UTCTime -> Int -> Int -> Handler Text
encryptBallot now winner loser = do
  key <- ballotKey <$> getYesod
  out <- liftIO $ WS.encryptIO key (encodeBallot (validTime `addUTCTime` now) winner loser)
  return . T.pack . BC.unpack $ out

decryptBallot :: UTCTime -> Text -> Handler (Int, Int)
decryptBallot now b = do
  key <- ballotKey <$> getYesod
  let Just b' = WS.decrypt key (BC.pack . T.unpack $ b)
      (expiry, winner, loser) = decodeBallot b'
  if expiry < now
    then error "expired ballot!"
    else return (winner, loser)
