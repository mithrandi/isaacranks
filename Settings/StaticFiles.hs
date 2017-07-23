module Settings.StaticFiles where

import Settings.Development (development)
import Yesod.EmbeddedStatic (mkEmbeddedStatic, embedDir, concatFiles)

mkEmbeddedStatic development "myStatic"
  [ embedDir "static"
  , concatFiles "css/app.css"
    [ "static/css/bootstrap.min.css"
    , "static/css/bootstrap-theme.min.css"
    , "static/css/font-awesome.min.css"
    , "static/css/icons.css"
    , "static/css/isaacranks.css"
    ]
  ]
