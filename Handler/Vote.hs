module Handler.Vote where

import           Data.Binary.Get (runGet, getWord32be)
import           Data.Binary.Put (runPut, putWord32be)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Lazy (toStrict, fromStrict)
import           Data.List (genericLength)
import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import           Import
import           Numeric (showFFloat)
import           System.Environment (getEnv)
import           System.Random (newStdGen)
import           System.Random.Shuffle (shuffle')
import           Vote (processVote)
import qualified Web.ClientSession as WS

getVoteR :: Handler Html
getVoteR = do
  items <- runDB $ selectList [] []
  gen <- lift newStdGen
  let (Entity _ left):(Entity _ right):_ = shuffle' items (length items) gen
  alreadyExpired
  ballotLeft <- lift $ encryptBallot (itemIsaacId left) (itemIsaacId right)
  ballotRight <- lift $ encryptBallot (itemIsaacId right) (itemIsaacId left)
  defaultLayout $ do
    setTitle "Isaac item ranks"
    $(widgetFile "vote")

encodeBallot :: Int -> Int -> B.ByteString
encodeBallot winner loser = toStrict . runPut $ do
  putWord32be (fromIntegral winner)
  putWord32be (fromIntegral loser)

decodeBallot :: B.ByteString -> (Int, Int)
decodeBallot = go . fromStrict
  where go = runGet $ do
          winner <- getWord32be
          loser <- getWord32be
          return (fromIntegral winner, fromIntegral loser)

maskingKey :: IO WS.Key
maskingKey = do
  Right key64 <- (B64.decode . BC.pack <$> getEnv "BALLOT_MASKING_KEY")
  let Right key = WS.initKey key64
  return key

encryptBallot :: Int -> Int -> IO Text
encryptBallot winner loser = do
  k <- maskingKey
  out <- WS.encryptIO k (encodeBallot winner loser)
  return . T.pack . BC.unpack $ out

decryptBallot :: Text -> IO (Int, Int)
decryptBallot b = do
  k <- maskingKey
  let Just b' = WS.decrypt k (BC.pack . T.unpack $ b)
  return $ decodeBallot b'

postVoteR :: Handler Html
postVoteR = do
  ballot <- runInputPost $ ireq textField "ballot"
  (winner, loser) <- lift $ decryptBallot ballot
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
