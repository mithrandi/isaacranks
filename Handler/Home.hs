module Handler.Home where

import Network.HTTP.Types (temporaryRedirect307)
import Import

getHomeR :: Handler Html
getHomeR = do
    redirectWith temporaryRedirect307 VoteR
