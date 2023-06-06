var Koa = require("koa");
var Router = require("koa-router");
const redis = require("redis");

const client = redis.createClient({
    url: process.env.REDIS_URL || "redis://localhost:6379",
});

client.on("connect", function () {
    console.log("Redis client connected");
});
client.on("error", function (err) {
    console.log("Something went wrong " + err);
});

async function run() {
    await client.connect();

    const app = new Koa();
    const router = new Router();

    router.get("/person/:person_name/address", async (ctx, next) => {
        console.log("recieved request", ctx.params);
        const value = await client.get("person:" + ctx.params.person_name);
        ctx.body = value || "neznámá adresa";
    });

    app.use(router.routes()).use(router.allowedMethods());
    app.listen(8080, () => {
        console.log("Access web server on http://localhost:8080/John");
    });
}
run();
