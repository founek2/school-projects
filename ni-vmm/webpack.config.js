const isEnvProduction = process.env.NODE_ENV === 'production';
const sourceMapEnv = process.env.SOURCE_MAP;

const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const CopyPlugin = require('copy-webpack-plugin');
const ReactRefreshWebpackPlugin = require('@pmmmwh/react-refresh-webpack-plugin');
const ReactRefreshTypeScript = require('react-refresh-typescript');
const ForkTsCheckerWebpackPlugin = require('fork-ts-checker-webpack-plugin');

const proxyTarget =
    process.env.PROXY === 'dev'
        ? 'https://dev.iotdomu.cz'
        : process.env.PROXY === 'prod'
        ? 'https://iotdomu.cz'
        : 'http://localhost:8085';

console.log('Proxy target: ', proxyTarget);
const config = {
    mode: isEnvProduction ? 'production' : 'development',
    entry: ['./src/index.tsx'],
    output: {
        path: path.resolve(__dirname, 'build'),
        filename: isEnvProduction ? 'assets/js/[name].[contenthash:8].js' : 'static/js/[name].js',
        chunkFilename: isEnvProduction ? 'assets/js/[name].[contenthash:8].chunk.js' : 'static/js/[name].chunk.js',
        assetModuleFilename: 'assets/media/[name].[hash][ext]',
        publicPath: '/',
        clean: true,
    },
    resolve: {
        extensions: ['.ts', '.tsx', '.js', '.jsx'],
        fallback: {
            stream: require.resolve('stream-browserify'),
            process: require.resolve('process'),
        },
    },
    module: {
        rules: [
            {
                test: /\.(ts|tsx|jsx|js)$/,
                include: [path.resolve('src')],
                exclude: /node_modules/,
                resolve: {
                    extensions: ['.ts', '.tsx', '.js', '.jsx', '.json'],
                },
                loader: 'ts-loader',
                options: {
                    getCustomTransformers: () => ({
                        before: [!isEnvProduction && ReactRefreshTypeScript()].filter(Boolean),
                    }),
                    transpileOnly: !isEnvProduction,
                },
            },
            {
                test: /\.css$/,
                use: [MiniCssExtractPlugin.loader, 'css-loader'],
                // use: {
                //     loader: MiniCssExtractPlugin.loader,
                // },
            },
            {
                test: /\.svg$/,
                use: ['@svgr/webpack'],
            },
            {
                test: /\.m?js$/,
            },
        ],
    },
    devtool: sourceMapEnv ? sourceMapEnv : isEnvProduction ? undefined : 'inline-source-map',
    // devtool: 'source-map',
    plugins: [
        !isEnvProduction && new ReactRefreshWebpackPlugin(),
        new ForkTsCheckerWebpackPlugin(),
        new CopyPlugin({
            patterns: [
                'public/robots.txt',
                {
                    from: 'public/*.svg',
                    to() {
                        return '[name][ext]';
                    },
                },
            ],
        }),
        new HtmlWebpackPlugin({
            template: path.join(__dirname, 'public/index.html'),
        }),
        new MiniCssExtractPlugin({
            filename: 'assets/css/[name].[contenthash:8].css',
            chunkFilename: 'assets/css/[name].[contenthash:8].chunk.css',
        }),
        new webpack.EnvironmentPlugin({ ...process.env }),
        new webpack.ProvidePlugin({
            process: 'process/browser',
        }),
        new webpack.HotModuleReplacementPlugin(),
        // new webpack.DefinePlugin({
        //     'process.env.NODE_ENV': process.env.NODE_ENV,
        // }),
    ].filter(Boolean),
    optimization: {
        splitChunks: {
            chunks: 'all',
            cacheGroups: {
                reactVendor: {
                    test: /[\\/]node_modules[\\/](react|react-dom|react-router-dom)[\\/]/,
                    name: 'vendor-react',
                    chunks: 'all',
                },
            },
        },
    },
};

module.exports = config;
