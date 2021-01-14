const path = require('path');
const CleanTerminalPlugin = require('clean-terminal-webpack-plugin');

module.exports = {
  entry: {
    contentScript: './contentScript/contentScript.js',
//    test: './test/test.js',
  },
  output: {
    filename: '[name].js',
    path: path.resolve(__dirname, 'dist'),
  },

  plugins: [new CleanTerminalPlugin()]
};
