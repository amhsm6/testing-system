const path = require("path");

module.exports = {
    entry: {
        index: "./src/index.js",
        course: "./src/course.js"
    },
    output: {
        filename: "[name].bundle.js",
        path: path.resolve(__dirname, "dist")
    },
    mode: "development",
    devServer: {
        host: "0.0.0.0",
        proxy: [
            {
                context: ["/api"],
                target: "http://127.0.0.1:1509"
            },
            {
                context: ["/api/submit"],
                target: "ws://127.0.0.1:1509",
                ws: true
            },
            {
                context: ["/course/*"],
                target: "http://127.0.0.1:8080",
                pathRewrite: { "/[0-9]+": ".html" }
            }
        ]
    }
};
