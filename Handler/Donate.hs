module Handler.Donate where

import Import

getDonateR :: Handler Html
getDonateR = defaultLayout $ do
  setTitle "Support this site"
  $(widgetFile "donate")
