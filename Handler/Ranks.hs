module Handler.Ranks (getRanksR) where

import Control.Lens (sumOf, folded)
import Data.List (genericLength)
import Import
import Model.IsaacVersion

getRanksR :: IsaacVersion -> Handler TypedContent
getRanksR ver = do
  items <- runDB $
    selectList [ItemVersion ==. ver] [Desc ItemRating]
  let items' = map entityVal items
      totalItems = genericLength items'
      votesCast = sumOf (folded . itemVotes) items' `div` 2
      meanVotes :: Double
      meanVotes = fromIntegral (votesCast * 2) / totalItems
  selectRep $ do
    provideRep . defaultLayout $ do
      setTitle "Isaac item ranks"
      addScript (StaticR js_bundle_js)
      $(widgetFile "ranks")
    provideJson $ object
      [ "items" .= items'
      , "votesCast" .= votesCast
      , "meanVotes" .= meanVotes
      ]
