module Vote
  ( processVote,
    reprocessVotes,
    encryptBallot,
    decryptBallot,
  )
where

import Conduit (foldlC, mapC)
import Control.Lens
import Control.Monad.Trans.Reader (ReaderT)
import Data.Binary.Get (getWord32be, runGet)
import Data.Binary.Put (putWord32be, runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Database.Persist.Sql (SqlBackend)
import Import hiding (toList)
import Model.IsaacVersion
import Numeric.LinearAlgebra
import Ranks
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
  deleteWhere [BallotTimestamp <=. (- validTime) `addUTCTime` now]
  return ()

type Matchup = (Key Item, Key Item)

type BeatMatrix = M.Map Matchup Int

beatMatrix :: Monad m => ConduitT Vote o m (M.Map IsaacVersion BeatMatrix)
beatMatrix = foldlC upd M.empty
  where
    upd bm v = bm & at ver . non M.empty . at (w, l) . non 0 +~ 1
      where
        w = v ^. voteWinner
        l = v ^. voteLoser
        ver = v ^. voteVersion

ranks :: BeatMatrix -> [Key Item] -> [(Key Item, R)]
ranks bm items =
  zip items . toList $ ilsrPairwise bm' 0.01
  where
    n = length items
    itemToIdx = M.fromList (zip items [0 ..])
    bm' = assoc (n, n) 0 (convR <$> M.toList bm)
    convR ((w, l), s) = ((itemToIdx M.! w, itemToIdx M.! l), fromIntegral s)

recalculatePairs :: (MonadIO m, MonadResource m) => M.Map (Key Item) Item -> M.Map IsaacVersion BeatMatrix -> ReaderT SqlBackend m ()
recalculatePairs items bms =
  ifor_ bms $ \ver bm ->
    for_ (ranks bm (itemsFor ver items)) $ \(itemId, rating) ->
      update
        itemId
        [ ItemRating =. rating,
          ItemVotes =. sumOf (ifolded . ifiltered (\(i1, i2) _ -> i1 == itemId || i2 == itemId)) bm
        ]
  where
    itemsFor ver = findIndicesOf ifolded (\i -> i ^. itemVersion == ver)

encodeBallot :: UTCTime -> Int -> Int -> B.ByteString
encodeBallot expiry winner loser = toStrict . runPut $ do
  putWord32be (truncate . utcTimeToPOSIXSeconds $ expiry)
  putWord32be (fromIntegral winner)
  putWord32be (fromIntegral loser)

decodeBallot :: B.ByteString -> (UTCTime, Int, Int)
decodeBallot = go . fromStrict
  where
    go = runGet $ do
      expiry <- getWord32be
      winner <- getWord32be
      loser <- getWord32be
      return
        ( posixSecondsToUTCTime (fromIntegral expiry),
          fromIntegral winner,
          fromIntegral loser
        )

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
  (expiry, winner, loser) <-
    case decodeBallot <$> (WS.decrypt key . BC.pack . T.unpack) b of
      Just r -> pure r
      Nothing -> error "invalid ballot!"
  if expiry < now
    then error "expired ballot!"
    else return (winner, loser)
