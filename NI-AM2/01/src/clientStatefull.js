const net = require("net");

const client = new net.Socket();

client.connect(8124, "127.0.0.1", function () {
    console.log("Connected");
    client.write("open");
});

client.on("data", function (data) {
    const message = data.toString();

    const d = new Date();
    console.log(`${d.getTime()}:${d.getMilliseconds()}`, "Data:" + message);

    if (message === "opened") {
        client.write("add");
    }
    if (message === "added") {
        client.write("process");
    }
    if (message === "processed") client.end();
});

client.on("close", function () {
    console.log("Connection closed");
});
