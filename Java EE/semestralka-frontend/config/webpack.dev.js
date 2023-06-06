const merge = require('webpack-merge');
const common = require('./webpack.common.js');
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

const mapStyle = process.env.MAP_STYLE === 'true';

module.exports = merge (common, {
    mode: 'development',
    devtool: 'inline-source-map',
    devServer: {
	    host: "0.0.0.0",
        port: 3042,
        historyApiFallback: true,
        overlay: true,
        open: true,
	   stats: 'errors-only',
	   proxy: {
		'/api/*': {
		  target: 'http://127.0.0.1:8080/todoList',
		  secure: false
		}
	   },
    },
    module: {
        rules: [
            {
                test: /\.css$/,
                use: [
                    { loader: "style-loader" },
                    { loader: mapStyle ? "css-loader?sourceMap" : "css-loader" }
                ]
            },
            {
                test: /\.s(a|c)ss$/,
                use: [
                    { loader: "style-loader" },
                    { loader: "css-loader" },
                    { loader: "sass-loader" }
                ]
            },
        ]
    },
    plugins: [
        new MiniCssExtractPlugin({
            filename: "[name].css",
        }),
    ],
});
