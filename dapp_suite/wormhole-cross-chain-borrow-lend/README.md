

## Getting Started

Included in this repository is:

- Example Solidity Code
- Example Forge local testing setup
- Testnet Deploy Scripts
- Example Testnet testing setup

### Environment Setup

- Node 16.14.1 or later, npm 8.5.0 or later: [https://docs.npmjs.com/downloading-and-installing-node-js-and-npm](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm)
- forge 0.2.0 or later: [https://book.getfoundry.sh/getting-started/installation](https://book.getfoundry.sh/getting-started/installation)

### Testing Locally

```bash
npm run build
forge test
```

### Deploying to Testnet

You will need a wallet with some testnet AVAX and testnet CELO. 

- [Obtain testnet AVAX here](https://core.app/tools/testnet-faucet/?token=C)
- [Obtain testnet CELO here](https://faucet.celo.org/alfajores)

```bash
EVM_PRIVATE_KEY=your_wallet_private_key npm run deploy
```

### Testing on Testnet

You will need a wallet with some testnet AVAX and testnet CELO - see above section for links to obtain this.

You must have also deployed contracts onto testnet (as described in the above section).

To test the cross chain borrow lending application, execute the test as such:

```bash
EVM_PRIVATE_KEY=your_wallet_private_key npm run test
```

**WARNING**: This repository has not been audited, so it may contain bugs and should be used for example purposes only. If you intend to use or build on this example to perform actual lending, it's highly recommended that you have the final code commit audited by a smart contract auditing firm.