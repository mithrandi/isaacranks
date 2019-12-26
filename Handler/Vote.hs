module Handler.Vote where

import qualified Data.ByteString.Char8 as BC
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import qualified Database.Esqueleto as E
import           Import
import           Instrument (observeHandlerL)
import           Model.IsaacVersion
import qualified Network.Wai as Wai
import           System.Random (newStdGen)
import           System.Random.Shuffle (shuffle')
import           Vote (processVote, encryptBallot, decryptBallot)

getVoteR :: IsaacVersion -> Handler TypedContent
getVoteR ver = observeHandlerL metricBallots (T.pack (show ver)) $ do
  items <- runDB $ E.select $ E.from $ \item -> do
    E.where_ $ item E.^. ItemVersion E.==. E.val ver
    return $ item E.^. ItemIsaacId
  gen <- liftIO newStdGen
  let E.Value leftId:E.Value rightId:_ = shuffle' items (length items) gen
  (Entity _ left, Entity _ right) <- runDB $
    (,) <$> getBy404 (UniqueItem ver leftId)
        <*> getBy404 (UniqueItem ver rightId)
  alreadyExpired
  addHeader "Cache-Control" "private, no-cache, must-revalidate"
  timestamp <- liftIO getCurrentTime
  ballotLeft <- encryptBallot timestamp leftId rightId
  ballotRight <- encryptBallot timestamp rightId leftId
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

postVoteR :: IsaacVersion -> Handler TypedContent
postVoteR ver = observeHandlerL metricVotes (T.pack (show ver)) $ do
  request <- waiRequest
  let value = T.pack . BC.unpack <$> lookup "X-Forwarded-For" (Wai.requestHeaders request)
      voter = maybe
              (T.pack . show $ Wai.remoteHost request)
              (T.stripStart . T.stripEnd . unsafeLast . T.splitOn ",")
              value
  ballot <- runInputPost $ ireq textField "ballot"
  timestamp <- liftIO getCurrentTime
  (winner, loser) <- decryptBallot timestamp ballot
  runDB (processVote ver winner loser timestamp voter ballot)
  getVoteR ver
