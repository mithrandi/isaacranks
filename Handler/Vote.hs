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
      showF x = showFFloat (Just 2) x ""
  items <- (ranks `zip`) <$> runDB (selectList [] [Desc ItemRating])
  let items' = map (entityVal . snd) items
      totalVotes = sum . map itemVotes $ items'
      votesCast = totalVotes `div` 2
      totalItems = genericLength items
      meanVotes :: Double
      meanVotes = fromIntegral totalVotes / totalItems
      meanRating :: Double
      meanRating = (sum . map itemRating) items' / totalItems
      minRating = minimum . map itemRating $ items'
      maxRating = maximum . map itemRating $ items'
      ratingRange = maxRating - minRating
      normalizedRating item = (itemRating item - minRating) / ratingRange * 1000
  defaultLayout $ do
    setTitle "Isaac item ranks"
    $(widgetFile "ranks")
