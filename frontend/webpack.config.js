const webpack = require('webpack')

module.exports = {
  context: __dirname + "/src",
  entry: "./main.js",
  output: {
    path: __dirname + "/dist",
    filename: "bundle.js"
  },
  module: {
    loaders: [
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        exclude: /node_modules/,
        query: {
        }
      }
    ]
  },
  plugins: [],
  devServer: {
    open: true,
  }
}
