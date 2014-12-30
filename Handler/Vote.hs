module Handler.Vote where

import Data.Time (getCurrentTime, UTCTime)
import Import
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

getVoteR :: Handler Html
getVoteR = do
  items <- runDB $ selectList [] []
  gen <- lift newStdGen
  let (Entity _ left):(Entity _ right):_ = shuffle' items (length items) gen
  defaultLayout $ do
    setTitle "Isaac item ranks"
    $(widgetFile "vote")


processVote :: Int -> Int -> UTCTime -> YesodDB App (Key Vote)
processVote w l timestamp = do
    Just (Entity winnerId winner) <- getBy $ UniqueItem w
    Just (Entity loserId loser) <- getBy $ UniqueItem l
    let wr = itemRating winner
        lr = itemRating loser
    update winnerId [ItemRating +=. adjustment 1.0 (expected wr lr)]
    update loserId [ItemRating +=. adjustment 0.0 (expected lr wr)]
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
  redirect VoteR

getRanksR :: Handler Html
getRanksR = do
  let ranks :: [Integer]
      ranks = [1..]
  items <- (ranks `zip`) <$> runDB (selectList [] [Desc ItemRating])
  defaultLayout $ do
    setTitle "Isaac item ranks"
    $(widgetFile "ranks")
