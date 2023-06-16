# Mantle Content Bounty Submission

## Making Healthcare Affordable and Accessible with Mantle Blockchain

Pdf format: https://github.com/aspiringsecurity/EMTTR/blob/main/dapp_suite/mantle-medical-data-security/Making%20Healthcare%20Affordable%20and%20Accessible%20with%20Mantle.pdf

Docx format: https://github.com/aspiringsecurity/EMTTR/blob/main/dapp_suite/mantle-medical-data-security/Making%20Healthcare%20Affordable%20and%20Accessible%20with%20Mantle.docx

# Data Security in EMTTR using Mantle

 We are ensuring data security in EMTTR using Mantle's asymmetric and symmetric encryption/decryption capabilities, mnemonic/account generation and management.

## Features
- React frontend responsible for encrypting data, generating user accounts and communicating with API
- API component responsible for communicating with our blockchain via web3 and storing contract information
- Contract component reponsible for compilation and deployment of "Notes" and "Users" contracts

## Setup

#### `docker-compose`	

To deploy the app to docker locally:	

#### From project root(/):	

> `docker-compose build` \
> `docker-compose up`

 The application will start up but your contracts will not be deployed to your parity instance, so HTTP requests will fail until you deploy the contracts.	

 #### From /contracts:	

> `npm i` \
> `npm run compile` \
> `npm run deploy`	

Unlike local development, data should persist while the parity volume (defined in `docker-compose.yml`) still exists.

The API server will start on port 3000 and react app on port 3001

#### `Local`

To set up the demo locally, perform the following actions:

#### From /api:

> `npm i`

#### From /contracts:

> `npm i` \
> `npm run compile` \
> `npm run parity` \
> `npm run deploy` (Deploys contracts to your running parity node)

#### From /react:

> `npm i`

#### From project root(/):

> `npm i` \
> `npm start`

The API server will start on port 3000 and react app on port 3001

Available API routes:


```js
GET http://localhost:3000/api/users
GET http://localhost:3000/api/notes
POST http://localhost:3000/api/notes
```
