# Axelar cross-chain dApp starter kit

## Introduction

This repo provides a basic template to quickly bootstrap your application using the [Axelar Local Development Environment](https://github.com/axelarnetwork/axelar-local-dev), which simulates the relay of messages cross-chain, on the backend.

The code provides a basic scaffold for:

-   Solidity contract templates (`contract_templates` directory)
-   a web interface (`web` directory)

Once ready, you can use this repo to deploy your contracts to testnet.

## One-time setup

Install [nodejs](https://nodejs.org/en/download/) version 16. Run `node -v` to check your installation. If needed you can switch your node version via
```bash
sudo npm i -g n
n v16.15.0
```

Clone this repo:

```bash
git clone https://github.com/axelarnetwork/axelar-dapp-starter-kit.git
```

Install dependencies:

```bash
npm update && npm install
```

## Build

#### LOCAL ENVIRONMENT

First, set up a `.env` file based on the `.env.sample` template in the root directory and update your local wallet for testing. Use either EVM_MNEMONIC or EVM_PRIVATE_KEY, but not both!

#### LOCAL SETUP

-   cd to the root directory of this project and run the command below.
-   NOTE: Leave this node running on a separate terminal before deploying and testing the dApps.

```bash
npm run run-local-env
```

#### BUILD LOCAL

Then, from a separate terminal, run the following command:

```bash
npm run build
npm run deploy <TEMPLATE_DIRECTORY> <ENVIRONMENT>

# e.g.
npm run deploy CallContractWithToken local
```

This will build your contracts in the `build` folder and the `web` directory for the UI build.

-   `info` for contract addresses by environment
-   `web/abi` for relevant ABIs, abd `web/info` for the network info for the UI

#### TEST

Option 1: Via command line, run:

```bash
npm run invoke-contract <TEMPLATE_DIRECTORY> <ENVIRONMENT> <SRC_CHAIN> <DEST_CHAIN> <AMOUNT> <ADDR_1> <ADDR_2> <OTHER_ADDRESSES>

# e.g.
npm run invoke-contract CallContractWithToken local Ethereum Avalanche 40 0x74Ccd7d9F1F40417C6F7fD1151429a2c44c34e6d 0x3B94CbD6d0f09db75435d6E3c9449a6B70BB55E2
```

Option 2: run your UI in the following steps:

-   In a separate terminal window, cd to the `web` directory
-   Set up a `.env.local` file based on the `.env.sample` template in the `web` directory and update either a NEXT_PUBLIC_EVM_MNEMONIC or NEXT_PUBLIC_EVM_PRIVATE_KEY (but not both) used for testing.
-   run `npm install`, then `npm run dev`
-   open browser at `http://localhost:3000`
-   enter params in the UI. in the text input, be sure to hit the "Enter" key after each destination address!

## DEPLOY

Once you are comfortable with your local dev scripts, deploying it to testnet should be simple:

```bash
npm run deploy <TEMPLATE_DIRECTORY> testnet

# e.g.
npm run deploy CallContractWithToken testnet
```

Ensure that the account tied to your EVM_MNEMONIC has enough funds to deploy to all supported EVM chains!
