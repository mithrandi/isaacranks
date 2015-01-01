module Vote where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Time (UTCTime)
import Database.Persist.Sql (SqlBackend)
import Import

processVote' :: MonadIO m => Entity Item -> Entity Item -> UTCTime -> Text -> Maybe Text -> ReaderT SqlBackend m Vote
processVote' (Entity winnerId winner) (Entity loserId loser) timestamp voter rawBallot = do
    let wr = itemRating winner
        lr = itemRating loser
    update winnerId [ItemRating +=. adjustment 1.0 (expected wr lr),
                     ItemVotes +=. 1]
    update loserId [ItemRating +=. adjustment 0.0 (expected lr wr),
                    ItemVotes +=. 1]
    return $ Vote winnerId loserId timestamp voter rawBallot
    where adjustment s e = k * (s - e)
          expected r1 r2 = 1 / (1 + 10 ** ((r2 - r1) / 400))
          k = 24.0

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

reprocessVotes :: MonadIO m => ReaderT SqlBackend m ()
reprocessVotes = do
  updateWhere [] [ItemRating =. 500, ItemVotes =. 0]
  votes <- selectList [] [Asc VoteTimestamp]
  mapM_ reprocessVote . map entityVal $ votes
