var path = require('path');
var webpack = require('webpack');


module.exports =
{ entry: path.resolve(__dirname)
, output:
  { path: path.resolve(__dirname, '..', 'static', 'js')
  , filename: 'bundle.js'
  }
, devtool: 'source-map'
, plugins:
  [ new webpack.ProvidePlugin(
      { 'fetch': 'imports?this=>global!exports?global.fetch!whatwg-fetch' })
  , new webpack.DefinePlugin(
      { 'process.env':
        { NODE_ENV: JSON.stringify('production')
      }
    })
  , new webpack.optimize.DedupePlugin()
  , new webpack.optimize.UglifyJsPlugin(
      { compress: { warnings: false }
      })
  ]
, module:
  { loaders:
    [ { test: /\.js$/
      , loader: ['babel']
      , query:
        { optional:
          [ 'runtime'
          , 'optimisation.react.constantElements'
          , 'optimisation.react.inlineElements'
          ]
        , stage: 0
        }
      , exclude: /node_modules/
      , include: __dirname
      }
    ]
  }
};
