module Items (loadData) where

import           Control.Lens
import           Control.Monad (forM_)
import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Default (def)
import           Data.Text.Lens (_Text)
import           Database.Persist.Sql (SqlBackend)
import           Filesystem.Path.CurrentOS (decodeString)
import           Import
import           Model.IsaacPool
import qualified Text.XML as XML
import           Text.XML.Lens (root, el, attr, attributeIs, (./))

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

loadData :: String -> FilePath -> ReaderT SqlBackend (LoggingT IO) ()
loadData ver p = do
  let Just ver' = fromPathPiece (_Text # ver)
  doc <- liftIO $ XML.readFile def (decodeString p)
  updateWhere [] [ItemPools =. []]
  forM_ pools $ \(name, pool) -> do
    forMOf_ (itemsFor name) doc $ \e -> do
      let isaacId :: Int
          isaacId = e ^?! attr "Id" . _Text . _Show
      Just (Entity k item) <- getBy $ UniqueItem ver' isaacId
      replace k $ item { itemPools = pool:(itemPools item) }
