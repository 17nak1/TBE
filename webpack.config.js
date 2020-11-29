var path = require('path');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');

module.exports = {
  entry: './src/index.js',
  output: {
    path: path.resolve(__dirname, 'www'),
    filename: 'js/bundle.js'
  },
  node: {
    fs: 'empty',
    path: 'empty'
  },
  devServer: {
    contentBase: path.join(__dirname, 'www'),
    publicPath: '/',
    watchContentBase: true,
    compress: true,
    port: 8080
  },
  module: {
    rules: [
      {
        test: /\.s[ac]ss$/i,
        use: [
          // Creates `style` nodes from JS strings
          MiniCssExtractPlugin.loader,
          // Translates CSS into CommonJS
          'css-loader?-url',
          // Compiles Sass to CSS
          'sass-loader',
        ],
      },
      {
        test: /\.wasm$/i,
        type: 'javascript/auto',
        use: 'arraybuffer-loader',
      }
    ],
  },
  plugins: [
    new MiniCssExtractPlugin({
      filename: 'styles/index.css'
    })
  ]
};