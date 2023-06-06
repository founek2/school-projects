const net = require("net");

const StatusState = {
    CLOSED: "closed",
    OPENED: "opened",
    ADDED: "added",
    PROCESSED: "processed",
};

let current = {};

const server = net.createServer(function (socket) {
    console.log("socket opened");
    socket.id = Date.now();
    current[socket.id] = { status: StatusState.CLOSED, items: [] };

    socket.setEncoding("utf8");

    socket.on("end", function () {
        console.log("connection/socket closed");
    });

    socket.on("data", function (data) {
        const message = data.toString();

        const d = new Date();
        console.log(`${d.getTime()}:${d.getMilliseconds()}`, "Data:" + message);
        currentState = current[socket.id];

        if (currentState.status === StatusState.CLOSED && message === "open") {
            currentState.status = StatusState.OPENED;
        } else if (currentState.status === StatusState.OPENED && message === "add") {
            currentState.status = StatusState.ADDED;
        } else if (currentState.status === StatusState.ADDED && message === "process") {
            currentState.status = StatusState.PROCESSED;
        } else return;

        socket.write(currentState.status);
        if (currentState.status === StatusState.PROCESSED) currentState.status = StatusState.CLOSED;
    });
});

server.listen(8124, function () {
    // start server (port 8124)
    console.log("server started");
});
