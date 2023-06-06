const Koa = require('koa');
const Router = require('@koa/router');
const bodyParser = require('koa-bodyparser');
const { transformToJson } = require('./transformation');

const app = new Koa();
const router = new Router();

const PORT = 3000;

router.post('/transform', (ctx) => {
    ctx.body = transformToJson(ctx.request.body);
});

app.use(
    bodyParser({
        enableTypes: ['text'],
    })
)
    .use(router.routes())
    .use(router.allowedMethods());

app.listen(PORT);
console.log(
    `Running at url http://localhost:${PORT}, make post to URL http://localhost:${PORT}/transform to recieve transformation of your data`
);
