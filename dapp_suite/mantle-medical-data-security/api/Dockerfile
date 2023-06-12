FROM node:10.12-alpine

RUN echo 1
RUN apk add --update --no-cache alpine-sdk git python openssl curl bash && \
  rm -rf /tmp/* /var/cache/apk/*

RUN npm i -g nodemon

USER node

RUN mkdir /home/node/api
WORKDIR /home/node/api

COPY package* ./

RUN npm i

COPY . /home/node/api

EXPOSE 3000 

CMD nodemon run.js
