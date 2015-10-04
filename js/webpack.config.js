var path = require('path');
var webpack = require('webpack');


module.exports =
{ entry: path.resolve(__dirname)
, output:
  { path: path.resolve(__dirname, '..', 'static', 'js')
  , filename: 'bundle.js'
  }
, devtool: '#cheap-module-eval-source-map'
, plugins:
  [ new webpack.ProvidePlugin(
      { 'fetch': 'imports?this=>global!exports?global.fetch!whatwg-fetch' })
  ]
, module:
  { loaders:
    [ { test: /\.js$/
      , loader: ['babel']
      , query:
        { optional: ['runtime']
        , stage: 0
        }
      , exclude: /node_modules/
      , include: __dirname
      }
    ]
  }
};
