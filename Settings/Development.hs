module Settings.Development (development, production) where

import Prelude

development :: Bool
development =
#ifdef DEVELOPMENT
  True
#else
  False
#endif

production :: Bool
production = not development
