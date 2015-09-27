module Handler.Vote where

import           Control.Lens hiding ((.=))
import           Data.Aeson (encode)
import           Data.Binary.Get (runGet, getWord32be)
import           Data.Binary.Put (runPut, putWord32be)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Lazy (toStrict, fromStrict)
import           Data.List (last)
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time (getCurrentTime)
import           Import
import           Model.IsaacVersion
import           Network.Wai (requestHeaders, remoteHost)
import           System.Random (newStdGen)
import           System.Random.Shuffle (shuffle')
import           Vote (processVote)
import qualified Web.ClientSession as WS

jsonToText :: ToJSON a => a -> Text
jsonToText = decodeUtf8 . toStrict . encode

getVoteR :: IsaacVersion -> Handler TypedContent
getVoteR ver = do
  items <- runDB $ selectList [ItemVersion ==. ver] []
  gen <- lift newStdGen
  let (Entity _ left):(Entity _ right):_ = shuffle' items (length items) gen
  alreadyExpired
  ballotLeft <- encryptBallot (left^.itemIsaacId) (right^.itemIsaacId)
  ballotRight <- encryptBallot (right^.itemIsaacId) (left^.itemIsaacId)
  let ballotJson = object
                   [ "left" .= left
                   , "ballotLeft" .= ballotLeft
                   , "right" .= right
                   , "ballotRight" .= ballotRight
                   ]
  selectRep $ do
    provideRep . defaultLayout $ do
      setTitle "Isaac item ranks"
      addScript (StaticR js_bundle_js)
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

postVoteR :: IsaacVersion -> Handler TypedContent
postVoteR ver = do
  request <- waiRequest
  let value = T.pack . BC.unpack <$> lookup "X-Forwarded-For" (requestHeaders request)
      voter = maybe
              (T.pack . show $ remoteHost request)
              (T.stripStart . T.stripEnd . last . T.splitOn ",")
              value
  (ballot, fancy) <- runInputPost $
                    (,) <$> ireq textField "ballot"
                        <*> ireq checkBoxField "fancy"
  (winner, loser) <- decryptBallot ballot
  timestamp <- lift getCurrentTime
  _ <- runDB (processVote ver winner loser timestamp voter ballot fancy)
  getVoteR ver
