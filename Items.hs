module Items (loadData) where

import           Control.Lens
import           Control.Monad (forM_)
import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Default (def)
import qualified Data.Text as T
import           Data.Text.Lens (_Text)
import           Database.Persist.Sql (SqlBackend)
import           Import
import           Model.IsaacPool
import           Model.IsaacVersion
import qualified Text.XML as XML
import           Text.XML.Lens (root, el, attr, attributeIs, attributeSatisfies, (./), localName)

pools :: [(Text, IsaacPool)]
pools =
  [ ("treasure", PoolItemRoom)
  , ("shop", PoolShop)
  , ("boss", PoolBossRoom)
  , ("devil", PoolDevilRoom)
  , ("angel", PoolAngelRoom)
  , ("secret", PoolSecretRoom)
  , ("library", PoolLibrary)
  , ("goldenChest", PoolGoldenChest)
  , ("redChest", PoolRedChest)
  , ("beggar", PoolBeggar)
  , ("demonBeggar", PoolDemonBeggar)
  , ("curse", PoolCurseRoom)
  , ("keyMaster", PoolKeyBeggar)
  ]

itemsFor :: Text -> Traversal' XML.Document XML.Element
itemsFor name = root . el "ItemPools" ./ el "Pool" . attributeIs "Name" name ./ el "Item"

items :: Traversal' XML.Document XML.Element
items = root ./ filtered ((`elem` ["active", "passive", "familiar"]) . XML.elementName) . attributeSatisfies "description" (not . T.null)

loadData :: String -> FilePath -> FilePath -> ReaderT SqlBackend (LoggingT IO) ()
loadData ver itemsPath poolsPath = do
  let Just ver' = fromPathPiece (_Text # ver) :: Maybe IsaacVersion
  itemsDoc <- liftIO $ XML.readFile def itemsPath
  forMOf_ items itemsDoc $ \item -> do
    let name = item ^?! attr "name"
        desc = item ^?! attr "description"
        iid :: Int
        iid = item ^?! attr "id" . _Text . _Show
        itype = item ^. localName
    upsert
      Item { _itemVersion = ver'
           , _itemIsaacId = iid
           , _itemName = name
           , _itemDescription = desc
           , _itemWiki = ""
           , _itemRating = 500.1
           , _itemVotes = 0
           , _itemPools = []
           , _itemItype = itype
           }
      [ ItemName =. name
      , ItemDescription =. desc
      , ItemItype =. itype
      ]
  poolsDoc <- liftIO $ XML.readFile def poolsPath
  updateWhere [] [ItemPools =. []]
  forM_ pools $ \(name, pool) ->
    forMOf_ (itemsFor name) poolsDoc $ \e -> do
      let isaacId :: Int
          isaacId = e ^?! attr "Id" . _Text . _Show
      Just (Entity k item) <- getBy $ UniqueItem ver' isaacId
      replace k (item & itemPools %~ (pool:))
