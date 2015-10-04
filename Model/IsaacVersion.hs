module Model.IsaacVersion (IsaacVersion(..)) where

import Data.Aeson.TH
import Data.Char (toLower)
import Data.Default (Default(..))
import Prelude
import Yesod

data IsaacVersion = IsaacRebirth
                  | IsaacAfterbirth
                  deriving (Show, Read, Eq, Ord)

derivePersistField "IsaacVersion"
$(deriveJSON defaultOptions{constructorTagModifier = map toLower . drop 5} ''IsaacVersion)

instance PathPiece IsaacVersion where
  toPathPiece = \case
    IsaacRebirth -> "rebirth"
    IsaacAfterbirth -> "afterbirth"
  fromPathPiece = \case
    "rebirth" -> Just IsaacRebirth
    "afterbirth" -> Just IsaacAfterbirth
    _ -> Nothing

instance Default IsaacVersion where
  def = IsaacRebirth
