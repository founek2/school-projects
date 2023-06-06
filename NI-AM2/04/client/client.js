const redis = require("redis");

const client = redis.createClient({
    url: process.env.REDIS_URL || "redis://localhost:6379",
});

client.on("connect", async function () {
    console.log("Redis client connected");

    await client.set("person:John", "Thakurova 9, 160 00, Prague");
    await client.set("person:Martin", "Letohradská 15, 166 00, Plzeň");

    await client.disconnect();
});

client.on("error", function (err) {
    console.log("Something went wrong " + err);
});

client.connect();
