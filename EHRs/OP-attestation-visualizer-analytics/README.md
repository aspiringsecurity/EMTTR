# Attestation Station Analytics on Optimism using an analytics engine

We are extending the OP starter template and OP Calc to develop Attestation Station Analytics

## Getting Started

### Install Node

[See here](https://nodejs.org/en/download/).
Note that you need Node at a later version than 14.18.0, or 16 and above.
These instructions were verified with Node 18.

### Install Foundry

You will need to install [Foundry](https://book.getfoundry.sh/getting-started/installation) to build your smart contracts.

1. Run the following command:

   ```sh
   curl -L https://foundry.paradigm.xyz | bash
   ```

1. Source your environment as requested by Foundry.

1. Run `foundryup`.

</details>

## Set up environment

### Get an Etherscan key

1. Register for [Etherscan on Optimism](https://explorer.optimism.io/register).
   This account is different from your normal Etherscan account.

1. Go to [the API keys page](https://explorer.optimism.io/myapikey) and click **Add** to create a new API key.

### Specify .env

You will first need to set up your `.env` to tell Forge where to deploy your contract.

1. Copy `.env.example` to `.env`.

   ```sh
   cp .env.example .env
   ```

1. Edit your `.env` to specify the environment variables.

   - `ETHERSCAN_API_KEY`: Your Etherscan API Key.

   - `FORGE_RPC_URL`: The RPC URL of the network to which you deploy.
     If you use [Alchemy](https://github.com/ethereum-optimism/optimism-tutorial/tree/main/ecosystem/alchemy), your URL will look like this: `https://opt-goerli.g.alchemy.com/v2/<Alchemy API Key>`

   - `FORGE_PRIVATE_KEY`: The private key of the wallet you want to deploy from.

   - `VITE_WALLETCONNECT_PROJECT_ID`: WalletConnect v2 requires a project ID. You can obtain it from your WC dashboard: https://docs.walletconnect.com/2.0/web/web3wallet/installation#obtain-project-id

## Start the application

<img width="450" alt="starter-app-screenshot" src="https://user-images.githubusercontent.com/389705/225778318-4e6fb8c0-c5d7-4aea-9fc2-2efd17ca435c.png">

1. Clone/fork the optimism-starter repo

   ```sh
   git clone https://github.com/ethereum-optimism/optimism-starter.git
   ```

1. Install the necessary node packages:

   ```sh
   cd optimism-starter
   npm install
   ```

1. Start the frontend with `npm run dev`

   ```sh
   npm run dev
   ```

   If you get errors during this step, you might need to [update your Foundry to the latest version](#install-foundry).

1. Open [localhost:5173](http://localhost:5173) in your browser.

   Once the webpage has loaded, changes made to files inside the `src/` directory (e.g. `src/App.tsx`) will automatically update the webpage.

See below for general usage instructions or [FAQ](./FAQ.md) for answers to general questions such as:

- [Where to get goerli eth]().
- [How to deploy a public version of your app](./FAQ.md#how-do-i-deploy-this).

## Generate ABIs & React Hooks

This project comes with `@wagmi/cli` built-in, which means you can generate wagmi-compatible (type safe) ABIs & React Hooks straight from the command line.

To generate ABIs & Hooks, follow the steps below.

## Generate code

To generate ABIs & React Hooks from your Foundry project (in `./contracts`), you can run:

```sh
npm run wagmi
```

This will use the wagmi config (`wagmi.config.ts`) to generate a `src/generated.ts` file which will include your ABIs & Hooks that you can start using in your project.

[Here is an example](https://github.com/ethereum-optimism/optimism-starter/blob/main/src/components/Attestoooooor.tsx#L77) of Hooks from the generated file being used.

## Deploying Contracts

To deploy your contracts to a network, you can use Foundry's [Forge](https://book.getfoundry.sh/forge/) – a command-line tool to tests, build, and deploy your smart contracts.

You can read a more in-depth guide on using Forge to deploy a smart contract [here](https://book.getfoundry.sh/forge/deploying), but we have included a simple script in the `package.json` to get you started.

Below are the steps to deploying a smart contract to Ethereum Mainnet using Forge:

## Deploy contract

You can now deploy your contract!

```sh
npm run deploy
```

## Developing with Anvil (Optimism Mainnet Fork)

Let's combine the above sections and use Anvil alongside our development environment to use our contracts (`./contracts`) against an Optimism fork.

### Start dev server

Run the command:

```sh
npm run dev:foundry
```

This will:

- Start a vite dev server,
- Start the `@wagmi/cli` in [**watch mode**](https://wagmi.sh/cli/commands/generate#options) to listen to changes in our contracts, and instantly generate code,
- Start an Anvil instance (Goerli Optimism Fork) on an RPC URL.

### Deploy our contract to Anvil

Now that we have an Anvil instance up and running, let's deploy our smart contract to the Anvil network:

```sh
npm run deploy:anvil
```

## Start developing

Now that your contract has been deployed to Anvil, you can start playing around with your contract straight from the web interface!

Head to [localhost:5173](http://localhost:5173) in your browser, connect your wallet, and try increment a counter on the Foundry chain. Use the generated code in `src/generated.ts` to do it and follow the [Attestooooor](https://github.com/ethereum-optimism/optimism-starter/blob/main/src/components/Attestoooooor.tsx) component as an example

> Tip: If you import an Anvil private key into your browser wallet (MetaMask, Coinbase Wallet, etc) – you will have 10,000 ETH to play with 😎. The private key is found in the terminal under "Private Keys" when you start up an Anvil instance with `npm run dev:foundry`.

