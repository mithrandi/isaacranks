module Vote where

import           Conduit (foldlC, mapC)
import           Control.Lens
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Binary.Get (runGet, getWord32be)
import           Data.Binary.Put (runPut, putWord32be)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Time (getCurrentTime, UTCTime, NominalDiffTime, addUTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           Database.Persist.Sql (SqlBackend)
import           Import
import           Model.IsaacVersion
import qualified Web.ClientSession as WS

processVote :: (MonadIO m) => IsaacVersion -> Int -> Int -> UTCTime -> Text -> Text -> ReaderT SqlBackend m ()
processVote ver w l timestamp voter rawBallot = do
  Entity w' _ <- getBy404 $ UniqueItem ver w
  Entity l' _ <- getBy404 $ UniqueItem ver l
  insert400_ $ Ballot rawBallot timestamp
  insert400_ $ Vote ver w' l' timestamp voter
  return ()

reprocessVotes :: (MonadIO m, MonadResource m) => ReaderT SqlBackend m ()
reprocessVotes = do
  items <- M.fromList . map (\(Entity a b) -> (a, b)) <$> selectList [] []
  bms <- runConduit $ selectSource [] [] .| mapC entityVal .| beatMatrix
  recalculatePairs items bms
  now <- liftIO getCurrentTime
  deleteWhere [BallotTimestamp <=. (-validTime) `addUTCTime` now]
  return ()

type Matchup = (Key Item, Key Item)
type BeatMatrix = M.Map Matchup Int

beatMatrix :: Monad m => ConduitT Vote o m (M.Map IsaacVersion BeatMatrix)
beatMatrix = foldlC upd M.empty
  where upd bm v = over (at ver . non M.empty . at (w, l) . non 0) (+1) bm
          where w = v ^. voteWinner
                l = v ^. voteLoser
                ver = v ^. voteVersion

ranks :: BeatMatrix -> [Key Item] -> [Key Item]
ranks bm items = smooth (sortOn (negate . wins) items)
  where wins i = length . filter (>0) . map (result i) $ items
        result i1 i2 = bm ^. at (i1, i2) . non 0
        smooth (i1:i2:is) | result i1 i2 < result i2 i1 = i2:smooth (i1:is)
                          | otherwise                   = i1:smooth (i2:is)
        smooth is = is

recalculatePairs :: (MonadIO m, MonadResource m) => M.Map (Key Item) Item -> M.Map IsaacVersion BeatMatrix -> ReaderT SqlBackend m ()
recalculatePairs items bms =
  ifor_ bms $ \ver bm ->
    ifor_ (ranks bm (itemsFor ver items)) $ \rank itemId ->
      update itemId [ ItemRating =. -(fromIntegral rank)
                    , ItemVotes =. sumOf (ifolded.ifiltered (\(i1, i2) _ -> i1 == itemId || i2 == itemId)) bm]
  where itemsFor ver = findIndicesOf ifolded (\i -> i ^. itemVersion == ver)

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
  key <- getsYesod appBallotKey
  out <- liftIO $ WS.encryptIO key (encodeBallot (validTime `addUTCTime` now) winner loser)
  return . T.pack . BC.unpack $ out

decryptBallot :: UTCTime -> Text -> Handler (Int, Int)
decryptBallot now b = do
  key <- getsYesod appBallotKey
  let Just b' = WS.decrypt key (BC.pack . T.unpack $ b)
      (expiry, winner, loser) = decodeBallot b'
  if expiry < now
    then error "expired ballot!"
    else return (winner, loser)
