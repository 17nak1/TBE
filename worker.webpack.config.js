var path = require('path');

module.exports = {
  mode: 'production',
  entry: './src/worker-index.js',
  output: {
    path: path.resolve(__dirname, "www", "js"),
    filename: 'worker-bundle.js'
  },
  node: {
    fs: 'empty',
    path: 'empty'
  },
  module: {
    rules: [
      {
        test: /\.wasm$/i,
        type: 'javascript/auto',
        use: 'arraybuffer-loader',
      }
    ]
  }
};