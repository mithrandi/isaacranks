module Handler.Home where

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import           Import
import           Network.HTTP.Types (temporaryRedirect307)
import           Yesod.Static (base64md5)

getHomeR :: Handler Html
getHomeR = do
    redirectWith temporaryRedirect307 VoteR

getMyFaviconR :: Handler ()
getMyFaviconR = do
  let path = "config/favicon.ico"
  setEtag . T.pack . base64md5 =<< lift (LB.readFile path)
  cacheSeconds (24 * 60 * 60)
  sendFile "image/x-icon" path

getChangesR :: Handler Html
getChangesR = defaultLayout $ do
  setTitle "Changelog"
  $(widgetFile "changes")
