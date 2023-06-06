var Koa = require("koa");
var Router = require("koa-router");

var app = new Koa();
var router = new Router();

router.get("/John", (ctx, next) => {
    console.log("recieved request");
    ctx.body = "Hello John";
});

app.use(router.routes()).use(router.allowedMethods());
app.listen(8888, () => {
    console.log("Access web server on http://localhost:8888/John");
});
