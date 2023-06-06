FROM node:16-alpine3.11

RUN apk add --no-cache yarn

RUN mkdir -p /var/www/app
WORKDIR  /var/www/app
COPY ./src/server.js package.json  ./

RUN yarn

EXPOSE 8888
CMD [ "node", "server.js" ]
