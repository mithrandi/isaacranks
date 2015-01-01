module Vote where

import           Control.Monad (forM_)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.List (foldl')
import qualified Data.Map.Strict as M
import           Data.Time (UTCTime)
import           Database.Persist.Sql (SqlBackend)
import           Debug.Trace (traceShow)
import           Import

processVote' :: MonadIO m => Entity Item -> Entity Item -> UTCTime -> Text -> Maybe Text -> ReaderT SqlBackend m Vote
processVote' (Entity winnerId winner) (Entity loserId loser) timestamp voter rawBallot = do
    let wr = itemRating winner
        lr = itemRating loser
    update winnerId [ItemRating +=. adjustment 1.0 (expected wr lr),
                     ItemVotes +=. 1]
    update loserId [ItemRating +=. adjustment 0.0 (expected lr wr),
                    ItemVotes +=. 1]
    return $ Vote winnerId loserId timestamp voter rawBallot

k :: Double
k = 24.0

adjustment :: Double -> Double -> Double
adjustment s e = k * (s - e)

expected :: Double -> Double -> Double
expected r1 r2 = 1 / (1 + 10 ** ((r2 - r1) / 400))

processVote :: MonadIO m => Int -> Int -> UTCTime -> Text -> Text -> ReaderT SqlBackend m (Key Vote)
processVote w l timestamp voter rawBallot = do
  Just w' <- getBy $ UniqueItem w
  Just l' <- getBy $ UniqueItem l
  vote <- processVote' w' l' timestamp voter (Just rawBallot)
  insert vote

reprocessVote :: MonadIO m => Vote -> ReaderT SqlBackend m ()
reprocessVote v = do
  let winnerId = voteWinner v
      loserId = voteLoser v
  Just winner <- get winnerId
  Just loser <- get loserId
  _ <- processVote' (Entity winnerId winner) (Entity loserId loser) (voteTimestamp v) (voteVoter v) (voteRawBallot v)
  return ()

applyVote :: M.Map (Key Item) Item -> Vote -> M.Map (Key Item) Item
applyVote items vote =
  adjustRating (adjustment 1.0 (expected wr lr)) w .
  adjustRating (adjustment 0.0 (expected lr wr)) l $ items
  where w = voteWinner vote
        l = voteLoser vote
        (Just winner) = M.lookup w items
        (Just loser) = M.lookup l items
        wr = itemRating winner
        lr = itemRating loser
        adjustRating a = M.adjust
                         (\i -> i {
                             itemRating = itemRating i + a,
                             itemVotes = itemVotes i + 1})

traceShowId :: Show b => b -> b
traceShowId a = traceShow a a

reprocessVotes :: (Functor m, MonadIO m) => ReaderT SqlBackend m ()
reprocessVotes = do
  updateWhere [] [ItemRating =. 500.1, ItemVotes =. 0]
  items <- M.fromList . map (\(Entity a b) -> (a, b)) <$> selectList [] []
  votes <- map entityVal <$> selectList [] [Asc VoteTimestamp]
  forM_ (M.toList $ foldl' applyVote items votes) $
    \(itemId, Item {itemRating = r, itemVotes = v}) -> update itemId [ItemRating =. r, ItemVotes =. v]
