const http = require("http");
const fs = require("fs");

const sendInterval = 5000;

function sendServerSendEvent(req, res) {
    res.writeHead(200, {
        "Content-Type": "text/event-stream",
        "Cache-Control": "no-cache",
        Connection: "keep-alive",
    });

    const sessionId = new Date().toLocaleTimeString();

    setInterval(function () {
        writeServerSendEvent(res, sessionId, new Date().toLocaleTimeString());
    }, sendInterval);

    writeServerSendEvent(res, sessionId, new Date().toLocaleTimeString());
}

function writeServerSendEvent(res, sessionId, data) {
    res.write(`id: ${sessionId}\n`);
    res.write("event: message\n");
    res.write("data: server event " + data + "\n\n");
}

function onRequest(req, res) {
    if (req.headers.accept && req.headers.accept == "text/event-stream") {
        if (req.url == "/events") {
            sendServerSendEvent(req, res);
        } else {
            res.writeHead(404);
            res.end();
        }
        return;
    }

    res.writeHead(200, { "Content-Type": "text/html" });
    res.write(fs.readFileSync(__dirname + "/index.html"));
    res.end();
}

http.createServer(onRequest).listen(8080, () => console.log("listening on http://localhost:8080"));
