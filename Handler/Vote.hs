module Handler.Vote where

import Data.List (genericLength)
import Data.Time (getCurrentTime, UTCTime)
import Import
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')
import Numeric (showFFloat)

getVoteR :: Handler Html
getVoteR = do
  items <- runDB $ selectList [] []
  gen <- lift newStdGen
  let (Entity _ left):(Entity _ right):_ = shuffle' items (length items) gen
  alreadyExpired
  defaultLayout $ do
    setTitle "Isaac item ranks"
    $(widgetFile "vote")

processVote :: Int -> Int -> UTCTime -> YesodDB App (Key Vote)
processVote w l timestamp = do
    Just (Entity winnerId winner) <- getBy $ UniqueItem w
    Just (Entity loserId loser) <- getBy $ UniqueItem l
    let wr = itemRating winner
        lr = itemRating loser
    update winnerId [ItemRating +=. adjustment 1.0 (expected wr lr),
                     ItemVotes +=. 1]
    update loserId [ItemRating +=. adjustment 0.0 (expected lr wr),
                    ItemVotes +=. 1]
    insert $ Vote winnerId loserId timestamp
    where adjustment s e = k * (s - e)
          expected r1 r2 = 1 / (1 + 10 ** ((r2 - r1) / 400))
          k = 24.0

postVoteR :: Handler Html
postVoteR = do
  (winner, loser) <- runInputPost $ (,)
                     <$> ireq intField "winner"
                     <*> ireq intField "loser"
  timestamp <- lift getCurrentTime
  _ <- runDB (processVote winner loser timestamp)
  getVoteR

getRanksR :: Handler Html
getRanksR = do
  let ranks :: [Integer]
      ranks = [1..]
  items <- (ranks `zip`) <$> runDB (selectList [] [Desc ItemRating])
  let totalVotes = (`div` 2) . sum . map (itemVotes . entityVal . snd) $ items
      meanVotes :: Double
      meanVotes = fromIntegral totalVotes / genericLength items
      meanVotesS = showFFloat (Just 2) meanVotes ""
  defaultLayout $ do
    setTitle "Isaac item ranks"
    $(widgetFile "ranks")
