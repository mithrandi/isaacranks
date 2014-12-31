module Handler.Home where

import Network.HTTP.Types (temporaryRedirect307)
import Import

getHomeR :: Handler Html
getHomeR = do
    redirectWith temporaryRedirect307 VoteR

getMyFaviconR :: MonadHandler m => m ()
getMyFaviconR = do
  cacheSeconds (24 * 60 * 60)
  sendFile "image/x-icon" "config/favicon.ico"
