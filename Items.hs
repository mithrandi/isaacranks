module Items (loadData) where

import           Control.Exception.Extra (retry)
import           Control.Lens
import           Control.Monad (unless)
import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.ByteString.Lens (unpackedChars)
import           Data.Default (def)
import qualified Data.Text as T
import           Data.Text.Lens (unpacked, _Text)
import           Data.Traversable (for)
import           Database.Persist.Sql (SqlBackend)
import           Import
import           Model.IsaacPool
import           Model.IsaacVersion
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as S
import qualified Text.XML as XML
import           Text.XML.Lens (root, el, attr, attributeIs, attributeSatisfies, (./), localName)
import           URI.ByteString (parseURI, strictURIParserOptions, pathL, URI(), serializeURIRef')

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

wikiBase :: URI
Right wikiBase = parseURI strictURIParserOptions
  "http://bindingofisaacrebirth.gamepedia.com/"

wikiName :: Text -> Int -> Text
wikiName name iid = case name of
  "<3" -> "Less_Than_Three"
  "1up!" -> "1up!"
  "Teleport!" -> "Teleport"
  "Odd Mushroom" -> case iid of
    120 -> "Odd_Mushroom_(Thin)"
    121 -> "Odd_Mushroom_(Large)"
    _ -> ""
  "PHD" -> "PHD"
  "A Pony" -> "The_Pony"
  "IV Bag" -> "IV_Bag"
  "Tooth Picks" -> "Toothpicks"
  "Guppy's Hairball" -> "Guppy's_Hair_Ball"
  "Humbleing Bundle" -> "Humbling_Bundle"
  "SMB Super Fan" -> "SMB_Super_Fan"
  "Spiderbaby" -> "Spider_Baby"
  "E. Coli" -> "E_Coli"
  "BBF" -> "BBF"
  "BFFS!" -> "BFFS!"
  "Lil Chest" -> "Lil'_Chest"
  "GB Bug" -> "GB_Bug"
  "PJs" -> "PJs"
  "Lil Loki" -> "Lil'_Loki"
  "Dark Princes Crown" -> "Dark_Prince's_Crown"
  "YO LISTEN!" -> "YO_LISTEN!"
  "Buddy in a Box" -> "Buddy_in_a_Box"
  "Mr. ME!" -> "Mr._ME!"
  _ -> T.replace "_The_" "_the_"
      . T.replace "_For_" "_for_"
      . T.replace "_Of_" "_of_"
      . T.replace "_To_" "_to_"
      . T.replace "'S" "'s"
      . T.replace " " "_"
      . T.toTitle $ name

loadData :: String -> FilePath -> FilePath -> ReaderT SqlBackend (LoggingT IO) ()
loadData ver itemsPath poolsPath = do
  let Just ver' = fromPathPiece (_Text # ver) :: Maybe IsaacVersion
  itemsDoc <- liftIO $ XML.readFile def itemsPath
  parsedItems <- liftIO $ S.withAPISession $ \sess ->
    for (itemsDoc ^.. items) $ \item -> do
      let name = item ^?! attr "name"
          desc = item ^?! attr "description"
          iid :: Int
          iid = item ^?! attr "id" . unpacked . _Show
          itype = item ^. localName
          wiki = wikiBase & pathL . unpackedChars . from unpacked <>~ wikiName name iid
          wikiS = wiki ^. to serializeURIRef' . unpackedChars
          wikiText :: Text
          wikiText = wikiS ^. from unpacked
      r <- retry 5 $ S.headWith
        (W.defaults & W.checkResponse ?~ \_ _ -> return ())
        sess
        (wiki ^. to serializeURIRef' . unpackedChars)
      unless (r ^. W.responseStatus . W.statusCode == 200) (print $ "Invalid wiki: " <> wikiText)
      return
        Item { _itemVersion = ver'
             , _itemIsaacId = iid
             , _itemName = name
             , _itemDescription = desc
             , _itemWiki = if r ^. W.responseStatus . W.statusCode == 200
                           then wikiText else ""
             , _itemRating = 500.1
             , _itemVotes = 0
             , _itemPools = []
             , _itemItype = itype
             }
  for_ parsedItems $ \parsedItem ->
    upsert parsedItem
    [ ItemName =. parsedItem ^. itemName
    , ItemDescription =. parsedItem ^. itemDescription
    , ItemWiki =. parsedItem ^. itemWiki
    , ItemItype =. parsedItem ^. itemItype
    ]
  poolsDoc <- liftIO $ XML.readFile def poolsPath
  updateWhere [] [ItemPools =. []]
  for_ pools $ \(name, pool) ->
    forOf_ (itemsFor name) poolsDoc $ \e -> do
      let isaacId :: Int
          isaacId = e ^?! attr "Id" . unpacked . _Show
      Just (Entity k item) <- getBy $ UniqueItem ver' isaacId
      replace k (item & itemPools %~ (pool:))
  updateWhere [ItemPools ==. []] [ItemPools =. [PoolMISC]]
