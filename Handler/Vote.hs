module Handler.Vote where

import           Control.Lens hiding ((.=))
import qualified Data.ByteString.Char8 as BC
import           Data.List (last)
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import           Import
import           Model.IsaacVersion
import           Network.Wai (requestHeaders, remoteHost)
import           System.Random (newStdGen)
import           System.Random.Shuffle (shuffle')
import           Vote (processVote, encryptBallot, decryptBallot)

getVoteR :: IsaacVersion -> Handler TypedContent
getVoteR ver = do
  items <- runDB $ selectList [ItemVersion ==. ver] []
  gen <- lift newStdGen
  let Entity _ left:Entity _ right:_ = shuffle' items (length items) gen
  alreadyExpired
  timestamp <- lift getCurrentTime
  ballotLeft <- encryptBallot timestamp (left^.itemIsaacId) (right^.itemIsaacId)
  ballotRight <- encryptBallot timestamp (right^.itemIsaacId) (left^.itemIsaacId)
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
postVoteR ver = do
  request <- waiRequest
  let value = T.pack . BC.unpack <$> lookup "X-Forwarded-For" (requestHeaders request)
      voter = maybe
              (T.pack . show $ remoteHost request)
              (T.stripStart . T.stripEnd . last . T.splitOn ",")
              value
  ballot <- runInputPost $ ireq textField "ballot"
  timestamp <- lift getCurrentTime
  (winner, loser) <- decryptBallot timestamp ballot
  runDB (processVote ver winner loser timestamp voter ballot)
  getVoteR ver
