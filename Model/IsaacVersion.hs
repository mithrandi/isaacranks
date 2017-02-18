module Model.IsaacVersion (IsaacVersion(..)) where

import Data.Aeson.TH
import Data.Char (toLower)
import Data.Default (Default(..))
import Prelude
import Yesod

data IsaacVersion = IsaacRebirth
                  | IsaacAfterbirth
                  | IsaacAfterbirthPlus
                  deriving (Show, Read, Eq, Ord)

derivePersistField "IsaacVersion"
$(deriveJSON defaultOptions{constructorTagModifier = map toLower . drop 5} ''IsaacVersion)

instance PathPiece IsaacVersion where
  toPathPiece = \case
    IsaacRebirth -> "rebirth"
    IsaacAfterbirth -> "afterbirth"
    IsaacAfterbirthPlus -> "afterbirthplus"
  fromPathPiece = \case
    "rebirth" -> Just IsaacRebirth
    "afterbirth" -> Just IsaacAfterbirth
    "afterbirthplus" -> Just IsaacAfterbirthPlus
    _ -> Nothing

instance Default IsaacVersion where
  def = IsaacAfterbirthPlus
