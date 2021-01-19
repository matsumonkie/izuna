const path = require('path');
const CleanTerminalPlugin = require('clean-terminal-webpack-plugin');
const glob = require('glob');

module.exports = {
  entry: glob.sync('./test/*.js'),
  output: {
    filename: '[name].js',
    path: path.resolve(__dirname, 'dist')
  },
  plugins: [new CleanTerminalPlugin()],
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /(node_modules|bower_components)/,
        loader: 'babel-loader',
        options: {
          presets: ["@babel/preset-env"],
          plugins: ["@babel/plugin-proposal-class-properties"]
        }
      }
    ]
  }
}
