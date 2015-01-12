module Handler.Vote where

import           Data.Aeson (encode)
import           Data.Binary.Get (runGet, getWord32be)
import           Data.Binary.Put (runPut, putWord32be)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Lazy (toStrict, fromStrict)
import           Data.List (genericLength)
import           Data.List (last)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time (getCurrentTime)
import           Import
import           Network.Wai (requestHeaders, remoteHost)
import           Numeric (showFFloat)
import           System.Random (newStdGen)
import           System.Random.Shuffle (shuffle')
import           Vote (processVote)
import qualified Web.ClientSession as WS

jsonToText :: ToJSON a => a -> Text
jsonToText = decodeUtf8 . toStrict . encode

getVoteR :: Handler TypedContent
getVoteR = do
  items <- runDB $ selectList [] []
  gen <- lift newStdGen
  let (Entity _ left):(Entity _ right):_ = shuffle' items (length items) gen
  alreadyExpired
  ballotLeft <- encryptBallot (itemIsaacId left) (itemIsaacId right)
  ballotRight <- encryptBallot (itemIsaacId right) (itemIsaacId left)
  let ballotJson = object
                   [ "left" .= left
                   , "ballotLeft" .= ballotLeft
                   , "right" .= right
                   , "ballotRight" .= ballotRight
                   ]
  selectRep $ do
    provideRep . defaultLayout $ do
      addScriptRemote "http://static.isaacranks.com/scripts/es5-shim-4.0.5"
      addScriptRemote "http://static.isaacranks.com/scripts/es5-sham-4.0.5"
      addScriptRemote "http://static.isaacranks.com/scripts/react-with-addons-0.12.2"
      addStylesheetRemote "http://static.isaacranks.com/styles/font-awesome-4.2.0"
      setTitle "Isaac item ranks"
      $(widgetFile "vote")
    provideJson ballotJson

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

encryptBallot :: Int -> Int -> Handler Text
encryptBallot winner loser = do
  k <- ballotKey <$> getYesod
  out <- liftIO $ WS.encryptIO k (encodeBallot winner loser)
  return . T.pack . BC.unpack $ out

decryptBallot :: Text -> Handler (Int, Int)
decryptBallot b = do
  k <- ballotKey <$> getYesod
  let Just b' = WS.decrypt k (BC.pack . T.unpack $ b)
  return $ decodeBallot b'

postVoteR :: Handler TypedContent
postVoteR = do
  request <- waiRequest
  let value = T.pack . BC.unpack <$> lookup "X-Forwarded-For" (requestHeaders request)
      voter = maybe
              (T.pack . show $ remoteHost request)
              (T.stripStart . T.stripEnd . last . T.splitOn ",")
              value
  ballot <- runInputPost $ ireq textField "ballot"
  (winner, loser) <- decryptBallot ballot
  timestamp <- lift getCurrentTime
  _ <- runDB (processVote winner loser timestamp voter ballot)
  getVoteR

getRanksR :: Handler Html
getRanksR = do
  let ranks :: [Integer]
      ranks = [1..]
      showF x = showFFloat (Just 2) x ""
  (items, votesCast) <- runDB $ (,)
    <$> ((ranks `zip`) <$> selectList [] [Desc ItemRating])
    <*> count ([] :: [Filter Vote])
  let items' = map (entityVal . snd) items
      totalItems = genericLength items
      meanVotes :: Double
      meanVotes = fromIntegral (votesCast * 2) / totalItems
      meanRating :: Double
      meanRating = (sum . map itemRating) items' / totalItems
      minRating = minimum . map itemRating $ items'
      maxRating = maximum . map itemRating $ items'
      ratingRange = maxRating - minRating
      normalizedRating item = (itemRating item - minRating) / ratingRange * 1000
  defaultLayout $ do
    setTitle "Isaac item ranks"
    $(widgetFile "ranks")
