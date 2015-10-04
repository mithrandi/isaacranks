module Handler.Ranks (getRanksR) where

import Control.Lens hiding ((.=))
import Data.List (genericLength)
import Data.Maybe (fromMaybe)
import Import
import Model.IsaacVersion

getRanksR :: IsaacVersion -> Handler TypedContent
getRanksR ver = do
  let ranks :: [Integer]
      ranks = [1..]
  (items, votesCast, dump) <- runDB $ (,,)
    <$> ((ranks `zip`) <$> selectList [ItemVersion ==. ver] [Desc ItemRating])
    <*> count ([] :: [Filter Vote])
    <*> selectFirst [] [Desc DumpTimestamp]
  let items' = map (entityVal . snd) items
      totalItems = genericLength items
      meanVotes :: Double
      meanVotes = fromIntegral (votesCast * 2) / totalItems
      minRating = fromMaybe 0 (minimumOf (traverse.itemRating) items')
      maxRating = fromMaybe 0 (maximumOf (traverse.itemRating) items')
      latestDump = dump ^? _Just . to entityVal . dumpPath . to ("http://static.isaacranks.com/" <>)
  selectRep $ do
    provideRep . defaultLayout $ do
      setTitle "Isaac item ranks"
      addScript (StaticR js_bundle_js)
      $(widgetFile "ranks")
    provideJson $ object
      [ "items" .= items'
      , "votesCast" .= votesCast
      , "meanVotes" .= meanVotes
      , "minRating" .= minRating
      , "maxRating" .= maxRating
      , "latestDump" .= latestDump
      ]
