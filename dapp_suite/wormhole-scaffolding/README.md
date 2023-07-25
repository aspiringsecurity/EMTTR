# Wormhole Scaffolding

This repository warehouses apps that integrate with Wormhole generic messaging and existing apps that use Wormhole message passing. These apps range in complexity and demonstrate how to organize your business logic in your smart contracts. These examples also show how you would write tests supporting your Wormhole integration.

## Prerequisites

### EVM

If your xChain app will require EVM smart contracts, we recommend using [Foundry tools](https://book.getfoundry.sh/getting-started/installation), which include `forge`, `anvil` and `cast` CLI tools.

### Worm CLI

First, checkout the [Wormhole](https://github.com/wormhole-foundation/wormhole) repo, then install the CLI tool by running:

```sh
wormhole/clients/js $ make install
```

`worm` is the swiss army knife for interacting with wormhole contracts on all
supported chains, and generating signed messages (VAAs) for testing.

## Build and Test

Each directory represents Wormhole integrations for specific blockchain networks. Please navigate
to a network subdirectory to see more details on building and testing.
