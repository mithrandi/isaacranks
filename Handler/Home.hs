module Handler.Home where

import qualified Data.ByteString.Lazy as LB
import           Data.Default (def)
import qualified Data.Text as T
import           Import
import           Network.HTTP.Types (temporaryRedirect307)
import           Yesod.Static (base64md5)

getHomeR :: Handler ()
getHomeR = redirectWith temporaryRedirect307 (VoteR def)

getDefaultVoteR :: Handler ()
getDefaultVoteR = redirectWith temporaryRedirect307 (VoteR def)

getDefaultRanksR :: Handler ()
getDefaultRanksR = redirectWith temporaryRedirect307 (RanksR def)

getMyFaviconR :: Handler ()
getMyFaviconR = do
  let path = "config/favicon.ico"
  setEtag . T.pack . base64md5 =<< lift (LB.readFile path)
  cacheSeconds (24 * 60 * 60)
  sendFile "image/x-icon" path
