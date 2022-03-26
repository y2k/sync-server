const path = require("path");

module.exports = {
    mode: "development",
    entry: "./src/WebApp.fs.js",
    output: {
        path: path.join(__dirname, "./public"),
        filename: "bundle.js",
    },
    devServer: {
        publicPath: "/",
        contentBase: "./public",
        port: 8090,
        proxy: {
            '/api': 'http://127.0.0.1:8080'
        }
    }
}