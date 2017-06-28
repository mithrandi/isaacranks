module Handler.Home where

import           Data.Default (def)
import           Import
import           Network.HTTP.Types (temporaryRedirect307)

getHomeR :: Handler ()
getHomeR = redirectWith temporaryRedirect307 (VoteR def)

getDefaultVoteR :: Handler ()
getDefaultVoteR = redirectWith temporaryRedirect307 (VoteR def)

getDefaultRanksR :: Handler ()
getDefaultRanksR = redirectWith temporaryRedirect307 (RanksR def)
