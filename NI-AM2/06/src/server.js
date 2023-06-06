const http2 = require("http2");
const fs = require("fs");
const path = require("path");
const mime = require("mime");

const { HTTP2_HEADER_PATH } = http2.constants;
// create a new server instance
const server = http2.createSecureServer(
    {
        key: fs.readFileSync("certs/key.pem"),
        cert: fs.readFileSync("certs/cert.pem"),
    },
    onRequest
);

// log any error that occurs when running the server
server.on("error", (err) => console.error(err));

function sendFile(stream, filePath) {
    const file = getFile(path.join(__dirname, filePath));
    if (!file) return stream.respond({ ":status": 404 });
    stream.respondWithFD(file.fileDescriptor, file.headers);
}

function push(stream, filePath) {
    const pushHeaders = { [HTTP2_HEADER_PATH]: filePath };

    stream.pushStream(pushHeaders, (err, pushStream) => {
        if (err) throw err;

        if (!pushStream.destroyed) {
            sendFile(pushStream, filePath);
        }
    });
}

function onRequest(req, res) {
    const filePath = req.headers[":path"] === "/" ? "/index.html" : req.headers[":path"];

    if (filePath === "/index.html") {
        push(res.stream, "/assets/kitten.jpeg");
        push(res.stream, "/assets/script.js");
        push(res.stream, "/assets/style.css");
    }

    sendFile(res.stream, filePath);
}

function getFile(filePath) {
    try {
        const fileDescriptor = fs.openSync(filePath, "r");
        const stats = fs.fstatSync(fileDescriptor);
        const contentType = mime.getType(filePath);
        console.log(filePath, contentType);
        return {
            fileDescriptor,
            headers: {
                "content-length": stats.size,
                "last-modified": new Date(stats.mtime).toUTCString(),
                "content-type": contentType,
            },
        };
    } catch (err) {
        console.log(err);
        return null;
    }
}
// start the server on port 8000
server.listen(8443, function () {
    console.log(`listening on https://localhost:8443`);
});
