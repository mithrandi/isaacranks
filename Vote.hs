module Vote where

import           Control.Lens
import           Control.Monad (forM_)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson.Lens (_JSON, _Object, _Array)
import           Data.List (foldl')
import qualified Data.Map.Strict as M
import           Data.Time (UTCTime)
import           Database.Persist.Sql (SqlBackend)
import           Debug.Trace (traceShow)
import           Import
import           Model.IsaacVersion

k :: Double
k = 24.0

adjustment :: Double -> Double -> Double
adjustment s e = k * (s - e)

expected :: Double -> Double -> Double
expected r1 r2 = 1 / (1 + 10 ** ((r2 - r1) / 400))

processVote :: MonadIO m => IsaacVersion -> Int -> Int -> UTCTime -> Text -> Text -> Bool -> ReaderT SqlBackend m (Key Vote)
processVote ver w l timestamp voter rawBallot fancy = do
  Just (Entity w' _) <- getBy $ UniqueItem ver w
  Just (Entity l' _) <- getBy $ UniqueItem ver l
  insert $ Vote ver w' l' timestamp voter (Just rawBallot) (Just fancy)

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

traceShowId :: Show b => b -> b
traceShowId a = traceShow a a

reprocessVotes ::
  (Functor m, MonadIO m) =>
  ReaderT SqlBackend m (M.Map (Key Item) Item, [Vote])
reprocessVotes = do
  updateWhere [] [ItemRating =. 500.1, ItemVotes =. 0]
  items <- M.fromList . map (\(Entity a b) -> (a, b)) <$> selectList [] []
  votes <- map entityVal <$> selectList [] [Asc VoteTimestamp]
  forM_ (M.toList $ foldl' applyVote items votes) $
    \(itemId, Item {_itemRating = r, _itemVotes = v}) -> update itemId [ItemRating =. r, ItemVotes =. v]
  return (items, votes)

serializeVotes :: M.Map (Key Item) Item -> [Vote] -> Value
serializeVotes items votes = _Array._Wrapped # map asJson votes
  where asJson vote = _Object._Wrapped #
                      [ ("version", vote^.voteVersion.re _JSON)
                      , ("winner", items^?!ix (vote^.voteWinner).itemIsaacId.re _JSON)
                      , ("loser", items^?!ix (vote^.voteLoser).itemIsaacId.re _JSON)
                      , ("timestamp", vote^.voteTimestamp.re _JSON)
                      ]
