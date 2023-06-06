var webpack = require('webpack');
var path = require('path');

const HtmlWebpackPlugin = require('html-webpack-plugin');
var BUILD_DIR = path.resolve(__dirname, 'build');
var APP_DIR = path.resolve(__dirname, 'src');
const isEnvProduction = process.env.NODE_ENV === 'production';

var config = {
    mode: isEnvProduction ? 'production' : 'development',
    entry: [APP_DIR + '/index.jsx'],

    resolve: {
        // modulesDirectories: ['node_modules'],
        extensions: ['', '.js', '.jsx', '.elm'],
    },

    module: {
        rules: [
            {
                test: /\.jsx?/,
                include: APP_DIR,
                exclude: [/elm-stuff/, /node_modules/],
                loader: 'babel-loader',
                options: {
                    presets: [['@babel/preset-env', { targets: 'defaults' }]],
                },
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: 'elm-webpack-loader',
            },
            {
                test: /\.css$/i,
                use: ['style-loader', 'css-loader'],
            },
        ],
    },
    output: {
        path: BUILD_DIR,
        filename: 'bundle.js',
    },
    plugins: [
        new HtmlWebpackPlugin({
            template: path.join(__dirname, 'public/index.html'),
        }),
    ],
};

module.exports = config;
