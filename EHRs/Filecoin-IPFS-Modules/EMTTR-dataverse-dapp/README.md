<br/>
<p align="center">
<a href=" " target="_blank">
<img src="./logo.svg" width="180" alt="Dataverse logo">
</a >
</p >
<br/>

# Create Dataverse App

The starter kit for developers to build their own application on top of [Dataverse](https://dataverse-os.com) operating system.

- Read [Developer Documentation](https://gitbook.dataverse-os.com/) to integrate [Runtime-SDK](https://github.com/dataverse-os/runtime-connector)
- Download [Data Wallet](https://github.com/dataverse-os/create-dataverse-app/releases/tag/DataWallet-0.5.33) to run OS Kernel and expose system calls to devs (Chrome version will be ready soon)

Note: Ensure your Metamask version is 10.28.3 or newer before you try data wallet.

# Getting Started

## Environment

Install the dependencies:

```bash
git clone https://github.com/dataverse-os/create-dataverse-app
cd create-dataverse-app
pnpm install
```

## Run demo
  
  ```bash
  pnpm dev
  ```
you can see the demo app running at http://localhost:5173.
<p align="center">
<a href=" " target="_blank">
<img src="https://s2.loli.net/2023/06/13/9B7TYLgSbcf1xHy.png" width="300" alt="Dataverse logo">
</a >
</p >

## Publish Your App

Set your dApp private key in `.env` and open `dataverse.config.ts` to check configurable variables:

```typescript
export const config = {
  slug: ...,
  name: ...,
  logo: ...,
  website: ...,
  defaultFolderName: ...,
  description: ...,
  models: [...],
  ceramicUrl: ...
};
```

These are basic information for your dApp, please update fields of `slug` and `name`. You can customize dApp's business logic with `models` field. Here is an example: 

```typescript
models: [
    {
      isPublicDomain: false,
      schemaName: "post.graphql",
      encryptable: ["text", "images", "videos"],
    },
    {
      isPublicDomain: true,
      schemaName: "profile.graphql",
    },
  ],
```

The `schemaName` links to the corresponding `models/_.graphql` file, defining your [ComposeDB](https://composedb.js.org/docs/0.4.x/guides/data-modeling/schemas) models & schemas. By default, you need to set `isPublicDomain=false` to ensure cross-app data security. If you set `isPublicDomain=true`, another dApp can compose this data model, indexing public data from your databases. 

You can also select which Ceramic endpoint your dApp is connecting to, to store data models and actual user data. App data on dataverse test ceramic node could be cleared regularly, so do NOT put anything important on test network. If you are running a production-ready dApp, you are suggested to run your own Ceramic node. You can deploy your own ceramic node using our tools [dapp-backend](https://github.com/dataverse-os/dapp-backend). Note that if your want to use your own ceramic, you need to ensure the url is accessible.

Finally you can publish your dApp: 

```bash
pnpm create-dataverse-app
```
This will deploy models to ceramic node you specify, and register data resources to DappTable. You can find resourceIDs in `output/app.json`, including your specific logic as well as file system. 

## Interact with Deployed App

We provide simple hooks & components in `src/`. Now run the frontend to interact with your dApp logic: 

```typescript
  // Connect user's wallet
  const { connectWallet } = useWallet();

  // Event streams
  const {
    pkh,
    createCapability,
    loadStreams,
    createPublicStream,
    createEncryptedStream,
    createPayableStream,
    monetizeStream,
    unlockStream,
    updateStream,
  } = useStream();
```

# Contributing

Contributions are always welcome! Open a PR or an issue!