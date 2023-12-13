# GOOGLE BIGQUERY WITH CHAINLINK FUNCTIONS

This project is configured to run with Chainlink Functions on the Polygon Mumbai testnet. Running on other testnets will require config to be added in `./networks.js`.

The architecture of Chainlink Functions is depicted below:
![Chainlink Architecture Diagram](https://user-images.githubusercontent.com/8016129/271724401-18d4326e-40d7-46ce-af52-12b8eacef347.png)

Note: this project is for workshop and demo purposes only. It is not appropriate for production use .

## Before you start...

Install [NodeJS V18](https://nodejs.org/en/download) or above. And Install [Deno]https://docs.deno.com/runtime/manual/getting_started/installation) - for eg on Mac, using homebrew you can run `brew install deno`.

Get at least 5 [LINK token](faucets.chain.link).
Get [Mumbai Matic](https://faucet.polygon.technology/)

Connect your Metamask wallet to Polygon Mumbai - click on [this link](https://chainlist.org/?search=mumbai&testnets=true) to have this done automatically.

### GCloud Setup

A service account is needed to use the googles BigQuery API even if it's a public dataset. The steps below will help you set one up.

1. **Go to the Google Cloud Console**: [https://console.cloud.google.com/](https://console.cloud.google.com/)
2. **Select your project**: From the top dropdown, choose the project in which you want to create the service account.
3. **Navigate to IAM & Admin**: In the left-hand navigation pane, click on "IAM & Admin", then select "Service accounts".
4. **Create Service Account**: Click on the "CREATE SERVICE ACCOUNT" button.
5. **Fill out the form**:
   - **Name**: Give your service account a name.
   - **Description**: Optionally, add a description.
   - Click "Create".
6. **Assign roles**: In the next step, you can assign roles to your service account. This determines the permissions the service account will have.
7. **Generate a key**: After roles have been assigned, you'll have the option to generate a JSON key for the service account, which can be used for authentication in applications and scripts.
8. Click "Done".

### GCP Service Account JSON keys

You should now have a JSON file like the example below

```json
{
  "type": "service_account",
  "project_id": "PROJECT_ID",
  "private_key_id": "JF9883U948U3FJIJSOIFJSID",
  "private_key": "-----BEGIN PRIVATE KEY-----\nPRIVATE_KEY\n-----END PRIVATE KEY-----\n",
  "client_email": "PROJECT_ID@PROJECT_ID.iam.gserviceaccount.com",
  "client_id": "34534534535",
  "auth_uri": "https://accounts.google.com/o/oauth2/auth",
  "token_uri": "https://oauth2.googleapis.com/token",
  "auth_provider_x509_cert_url": "https://www.googleapis.com/oauth2/v1/certs",
  "client_x509_cert_url": "https://www.googleapis.com/robot/v1/metadata/x509/PROJECT_ID%40PROJECT_ID.iam.gserviceaccount.com",
  "universe_domain": "googleapis.com"
}
```

If you intend to save this file in your project repo make sure you rename it to have a `*-secrets.json` suffix as files with this suffix are `gitignore`d.

You will be using the following keys as secrets in the next section for your Chainlink Function `project_id`, `private_key`, `client_email`

# Project Setup Steps

1. Run `npm install` to install the Chainlink Tooling, Hardhat tooling and other dependencies.

2. Have the values following ENV VARS ready for you to use.

```
POLYGON_MUMBAI_RPC_URL
RPC_URL= # This will also be the polygon mumbai rpc url
PRIVATE_KEY= # This is your dev wallet private key
GOOGLE_PROJECT_ID= # This is the "project_id" for the service account
GOOGLE_ISS= # This is the "client_email" address for the service account
GOOGLE_KEY= # This is the "private_key" for the service account
POLYGONSCAN_API_KEY # Optional if you'd like to verify the FunctionsBiqQueryConsumer Contract on the blockexplorer.
```

2. run `npx env-enc set-pw` and enter a memorable password that you can use to decrypt and view your secrets at rest. This password is the only way you can decrypt your `.env.enc` file. If you forget it you will need to delete the `.env.enc` file and repeat the process of setting up your env vars.

3. set each env var key-value pair by running `npx env-enc set` then putting in the env var name (eg: `PRIVATE_KEY`) then, when prompted, pasting in the secret's value. Read the prompts carefully to set all your environment variables correctly.

4. Your encrypted secrets are now stored in your project in the `.env.enc` file. This is `gitignore`d. You can view the decrypted version of your secrets with `npx env-enc view`.

# Project Run Steps

1. Go to the [Functions Subscriptions App](https://functions.chain.link). Connect your wallet to Polygon Mumbai on the Functions web app. Create your first subscription. This include two transactions:

- one to accept the Terms of Service that adds your wallet address to the Allowlist, and
- the other to create your Functions Subscription on-chain.

Take a note of your Subscription Id as you will need it when using Chainlink Functions programmatically.

Once a Functions Subscription is created you can manage it from the UI.

Please add at least 5 LINK to your subscription to run this project's code. This can be done from the `ACTIONS` button when your wallet is connected.

2.  Next we deploy the `FunctionsBigQueryConsumer.sol` smart contract on to the Mumbai testnet. Optionally, if we have set the `POLYGONSCAN_API_KEY` environment variable, we can pass the `--verify true` flag to verify our smart contract so we can interact with it via the blockexplorer's UI.

`npx hardhat deploy-consumer --network polygonMumbai --verify false`

Take a note of the contract `address` from your console - we will need it in the next step.

3. Next, we add this consumer contract as an authorized consumer against our subscription so that Chainlink Functions will know that the consumer contract is authorized to use our subscription balance. We do this by running:

`npx hardhat add-consumer --contract 0x__YOUR__CONTRACT__ADDRESS --subid <<SUBSCRIPTION_ID>> --network polygonMumbai`

If you want to check the status of your subscription at any time, including all consumers added, its balance etc, just run `npx hardhat sub-info --subid <<SUBSCRIPTION_ID>> --network polygonMumbai`

4. The custom JS code you will execute with Chainlink Functions script is in `/BigQuery-weather.js`. This code needs API keys to interact with BiqQuery. These Secrets/API keys needed for the decentralized execution of the custom JavaScript can be hosted temporarily in encrypted form in the nodes in the Chainlink Decentralized Network. So encrypt and upload secrets to don with:

`npx hardhat encrypt-secrets --network polygonMumbai`

This will prepare and encrypted your secrets for sending to the DON where it can be decrypted during runtime execution.

Additionally, the encrypted secrets are saved to `offchain-encrypted-secrets.json` in your project (gitignored) in an object that looks like:

```
{"encryptedSecrets":"0x7b2254.....encrypted----secrets--4778527363446f5057227d"}
```

The encrypted secrets will be read from this file in the next step.

5. We want to upload the encrypted secrets to the DON where they're hosted and then injected into your custom JS source during runtime. The DON will expire these secrets after a specified amount of time. Optionally, you can add the `--ttl` flag to set the expiry time in minutes. You can also pass the `--slotid` flag to specify which storage slot in the DON to allocate for your secrets.

The following script assigns default values of 60 (minutes) and `0` as the `slotid`.

`npx hardhat upload-to-don --network polygonMumbai`

Take a note of the secrets reference hex string. We will need this shortly.

6. Now we are ready run the Functions request! We send our custom secret and the other parts of the `requestConfig` object in `./request-config.js` to our Functions Big Query Consumer contract, which in turn initiates the Chainlink Functions Request.

**Note:** The default command below will run a local simulation of the custom script in `./BigQuery-weather.js` - including HTTP requests - so you can see what the expected response will be.However if you do not have Deno installed you should set the `--simulate false` flag.


`npx hardhat request-weather --network polygonMumbai --subid <<SUBSCRIPTION_ID>> --contract 0x__YOUR__CONTRACT__ADDRESS --secretsref 0X_SECRETS_REF`

Unless you pass `--simulate false` the local simulator included in the Chainlink Functions NPM package runs your custom JS code locally, including the HTTP requests to the Google BigQuery service. The response from the simulated script execution is printed to your console. Then, the request is submitted as an on-chain Transaction, which sends your source code and other request parameters to your `FunctionsBigQueryConsumer` contract, and from there Chainlink's decentralized oracle networks will execute the source code and return the response to your consumer contract.

The unique ID of the request is printed on screen.

7. Read the response on-chain to get the latest weather in Barcelona from a BigQuery weather dataset!  
   `npx hardhat read-weather --contract 0x__YOUR__CONTRACT__ADDRESS --network polygonMumbai`

## Understanding the Functions Consumer Contract

The Functions Consumer contract `./contracts/FunctionsBigQueryConsumer.sol` has the following public storage variables

```
bytes32 public s_lastRequestId;
bytes public s_lastError;
bytes public s_lastWeather;
```

These store the latest request Id, the latest error bytes (if any) or the latest response (in bytes) which is the latest Weather data pulled from Google BigQuery's data set.

You can also call `getCurrentTemperature()` to return the current temperature (Fahrenheit) expressed in 18 decimals (like Wei!).

This consumer contract is entered from `sendRequest()` which take all the relevant request config parameters, builds a Functions Request object, encodes it into CBOR and then sends the request CBOR to the Chainlink Oracle Contracts via the inherited `_sendRequest()` method.

The request config is found in the `./request-config.js` file.  For this workshop a single query is supplied as an argument to Chainlink Functions:
```sql
SELECT * FROM bigquery-public-data.noaa_gsod.gsod2023 where stn = '081810' order by date desc limit 1
```
This will get the latest temp data for the station `081810` which is the station for the city of `Barcelona`.


To examine the data on GCP and test out queries before running it live, visit the dataset at [https://console.cloud.google.com/marketplace/product/noaa-public/gsod](https://console.cloud.google.com/marketplace/product/noaa-public/gsod) and you can run the SQL query on [the BigQuery UI.](https://console.cloud.google.com/bigquery?p=bigquery-public-data&d=noaa_gsod&page=dataset&project=sc2023-399603&ws=!1m9!1m4!1m3!1ssc2023-399603!2sbquxjob_1ab96546_18aba1ccdda!3sUS!1m3!3m2!1sbigquery-public-data!2snoaa_gsod)
