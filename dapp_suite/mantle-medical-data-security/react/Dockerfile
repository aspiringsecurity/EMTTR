FROM node:10.12-alpine

RUN apk add --update --no-cache alpine-sdk git python openssl curl bash && \
  rm -rf /tmp/* /var/cache/apk/*

USER node

RUN mkdir /home/node/react
WORKDIR /home/node/react

COPY package* ./

RUN npm i

COPY . /home/node/react/

EXPOSE 3001

CMD npm start