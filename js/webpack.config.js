var path = require('path');
var webpack = require('webpack');


module.exports = function(env) {
    return (
    { entry: path.resolve(__dirname)
    , output:
      { path: path.resolve(__dirname, '..', 'static', 'js')
      , filename: 'bundle.js'
      }
    , devtool: env == 'production' ? 'source-map' : 'cheap-module-eval-source-map'
    , plugins:
      [ new webpack.ProvidePlugin(
          { 'fetch': 'imports-loader?this=>global!exports-loader?global.fetch!whatwg-fetch' })
      ]
    , module:
      { rules:
        [ { test: /\.js$/
          , loader: 'babel-loader'
          , exclude: /node_modules/
          , include: __dirname
        }
        ]
      }
    });
}
