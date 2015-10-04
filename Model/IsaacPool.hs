module Model.IsaacPool (IsaacPool(..)) where

import Data.Aeson.TH
import Prelude
import Yesod

data IsaacPool = PoolItemRoom
               | PoolShop
               | PoolBossRoom
               | PoolDevilRoom
               | PoolAngelRoom
               | PoolSecretRoom
               | PoolLibrary
               | PoolGoldenChest
               | PoolRedChest
               | PoolCurseRoom
               | PoolBeggar
               | PoolDemonBeggar
               | PoolKeyBeggar
               | PoolMISC
               deriving (Show, Read, Eq, Ord)

derivePersistField "IsaacPool"
$(deriveJSON defaultOptions{constructorTagModifier = drop 4} ''IsaacPool)
