module Settings.StaticFiles where

import Settings.Development (development)
import Yesod.EmbeddedStatic (mkEmbeddedStatic, embedDir)

mkEmbeddedStatic development "myStatic" [embedDir "static"]
