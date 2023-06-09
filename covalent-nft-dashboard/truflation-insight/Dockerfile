FROM node:18-alpine

RUN mkdir -p /home/node/app/node_modules && chown -R node:node /home/node/app

WORKDIR /home/node/app

ARG inflationAddress=0x5fc949612bCf622A63C4D66B1aA132728Cc0eb1C
ARG apiAddress=0x8a88122D96468B1c362Af6E6e0AA7c63a62892b7

COPY *.js *.json yarn.lock *.sh ./
USER node
COPY --chown=node:node . .
RUN chmod a+x install.sh ; ./install.sh ${inflationAddress} ${apiAddress}
EXPOSE 9011

CMD [ "yarn", "serve" ]
