{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Model where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Database.Persist.Quasi
import Model.IsaacVersion
import Model.IsaacPool
import Prelude
import Yesod

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [ mkPersist sqlSettings { mpsGenerateLenses = True }
      , mkMigrate "migrateAll"
      ]
    $(persistFileWith lowerCaseSettings "config/models")
