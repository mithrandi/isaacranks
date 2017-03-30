module Handler.Ranks (getRanksR) where

import Data.List (genericLength)
import Import
import Model.IsaacVersion

getRanksR :: IsaacVersion -> Handler TypedContent
getRanksR ver = do
  let ranks :: [Integer]
      ranks = [1..]
  (items, votesCast) <- runDB $ (,)
    <$> ((ranks `zip`) <$> selectList [ItemVersion ==. ver] [Desc ItemRating])
    <*> count [VoteVersion ==. ver]
  let items' = map (entityVal . snd) items
      totalItems = genericLength items
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
