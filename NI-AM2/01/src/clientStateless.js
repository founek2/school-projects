const net = require("net");

const client = new net.Socket();

client.connect(8124, "127.0.0.1", function () {
    console.log("Connected");
    client.write("open");
});

let orderId = -1;

client.on("data", function (buffer) {
    const message = buffer.toString();
    const [event, data] = message.split(":");

    const d = new Date();
    console.log(`${d.getTime()}:${d.getMilliseconds()}`, `event=${event}`, `data=${data}`);

    if (event === "opened") {
        orderId = data;
        client.write(`add:${orderId}:founek`);
        // client.write(`add:${orderId}`);
    }
    if (event === "added") {
        client.write(`process:${orderId}`);
    }
    if (event === "processed") client.end();
});

client.on("close", function () {
    console.log("Connection closed");
});
