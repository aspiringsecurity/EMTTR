# Ocean Storm by InsureBlox

Ocean Storm is a parametric insurance solution ☂&nbsp;&nbsp;for ships 🚢 &nbsp;&nbsp;going through rough seas 🌊 &nbsp;&nbsp;based on Blockchain 🚀&nbsp;&nbsp;i.e. hazzle-free, automatic pay-outs in case of adverse weather events.

### Get started

Install hardhat
`npm install --save-dev hardhat`

Export your [private key](https://metamask.zendesk.com/hc/en-us/articles/360015289632-How-to-Export-an-Account-Private-Key) and get Alchemy API key on `Kovan` (testnet with faucet system currently working and compatible with Keepers).
Create `.env` file with the following properties:

```
ALCHEMY_API_KEY = XXXXXXXXXX
PRIVATE_KEY = XXXXXXXX
```

Try running some of the following tasks:

```shell
npx hardhat accounts
npx hardhat compile
npx hardhat clean
npx hardhat test
npx hardhat node
node scripts/sample-script.js
npx hardhat help
```

### Running locally

1. `npx hardhat node` or `npx hardhat node --hostname 127.0.0.1`
2. `npx hardhat run scripts/deploy.js --network localhost`

> ℹ️  Connect metamask to Localhost:8545: RPC URL: http://localhost:8545 | Chain ID: 31337

### Deploy

`npx hardhat run scripts/deploy.js --network kovan`

### Frontend

Add and `.env` under `/frontend` folder with the following instruction:

`SKIP_PREFLIGHT_CHECK=true` 

See more [details](https://newbedev.com/javascript-skip-preflight-check-true-to-an-env-file-in-your-project-code-example)

```shell
cd frontend
npm install
npm start
```

### External Adapter

Refer to [Datalistic / Storm Glass External Adapter](https://github.com/InsureBlox/Datalistic_StormGlass_EA_chainlink)

### Test

Run the following command to perform the unit tests:
`npx hardhat test`

**Test coverage**

`npx hardhat coverage`
