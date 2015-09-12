module Model.IsaacVersion where

import Data.Aeson.TH
import Data.Char (toLower)
import Data.Default (Default(..))
import Prelude
import Yesod

data IsaacVersion = IsaacRebirth
                  | IsaacAfterbirth
                  deriving (Show, Read, Eq, Ord)

derivePersistField "IsaacVersion"
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''IsaacVersion)

instance PathPiece IsaacVersion where
  toPathPiece = \case
    IsaacRebirth -> "rebirth"
    IsaacAfterbirth -> "afterbirth"
  fromPathPiece = \case
    "rebirth" -> Just IsaacRebirth
    "afterbirth" -> Just IsaacAfterbirth
    _ -> Nothing

instance Default IsaacVersion where
  def = IsaacAfterbirth
