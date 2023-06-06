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

    socket.setEncoding("utf8");

    socket.on("end", function () {
        console.log("connection/socket closed");
    });

    socket.on("data", function (buffer) {
        const message = buffer.toString();
        const [action, orderId, data] = message.split(":");

        const d = new Date();
        console.log(
            `${d.getTime()}:${d.getMilliseconds()}`,
            `action=${action}`,
            `orderId=${orderId}`,
            `message=${message}`
        );
        currentState = current[orderId] || {};
        console.log("current state", currentState);

        if (currentState.status === undefined && action === "open") {
            const newOrderId = String(Date.now());
            current[newOrderId] = { status: StatusState.OPENED, items: [] };
            socket.write(current[newOrderId].status + ":" + newOrderId);
        } else if (currentState.status === StatusState.OPENED && action === "add") {
            current[orderId].status = StatusState.ADDED;
            current[orderId].items.push(data);
            socket.write(current[orderId].status + ":" + current[orderId].items.length);
        } else if (currentState.status === StatusState.ADDED && action === "process") {
            current[orderId].status = StatusState.PROCESSED;
            socket.write(current[orderId].status);
            current[orderId].status = StatusState.CLOSED;
        } else return;
    });
});

server.listen(8124, function () {
    // start server (port 8124)
    console.log("server started");
});
