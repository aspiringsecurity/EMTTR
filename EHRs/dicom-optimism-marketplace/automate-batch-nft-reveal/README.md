# Batch NFT Reveal in DICOM's NFT marketplace

The reveal process is automated and decentralized by having [Chainlink Automation](https://automation.chain.link) call the reveal function when DICOM images are validated with the requisite security parameters. We are extending Chainlink automation template for batch size reveal and our three configurable criteria are batch size, time interval, .dicom extension for the requisite dataset.



# Setup Process in DICOM's NFT marketplace

Begin by setting the parameters for the NFT collection as environment variables:

| Name                     | Description                                                                                                                                                                                                                                  |
| ------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `NFT_NAME `              | Name of the collection.                                                                                                                                                                                                                      |
| `NFT_SYMBOL `            | Symbol of the collection. Usually a few capital case letters.                                                                                                                                                                                |
| `NFT_MAX_SUPPLY `        | Maximum amount of tokens that can be minted.                                                                                                                                                                                                 |
| `NFT_MINT_COST `         | Cost to mint a single NFT in Ether.                                                                                                                                                                                                          |
| `NFT_REVEAL_BATCH_SIZE ` | Minimum number of newly minted tokens required to trigger metadata reveal of a new batch. Set `0` to disable.                                                                                                                                |
| `NFT_REVEAL_INTERVAL `   | Minimum time required to pass (in seconds) since last metadata reveal to trigger new batch reveal. It's recommended to be combined with batch size param to prevent unefficient batch revealing (ex: the minumum 1 NFT). Set `0` to disable. |

Note: Either batch size or time interval params must be set or metadata won't be revealed. It's also possible for the contract owner to change these params after contract deployment via the setter functions.

Additionally, you need to set variables required for the Hardhat project:

| Name                | Description                                                                                                                                                          |
| ------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `NETWORK_RPC_URL`   | Required to deploy to public networks. You can obtain this from websites like [Infura](https://infura.io), [Alchemy](https://www.alchemy.com) or host your own node. |
| `PRIVATE_KEY`       | Deployer's account private key. Can be exported from wallets, i.e. MetaMask.                                                                                         |
| `ETHERSCAN_API_KEY` | Verify contract code on Etherscan.                                                                                                                                   |

The easiest way to complete this step is by copying the `.env.example` file into `.env` and replacing the values with your own.

## Test

To run the unit tests on the local Hardhat network:

```bash
yarn test
```

## Deploy

To deploy the collection to a network, run:

```bash
yarn deploy <network>
```

The deploy scripts utilize an additional config (`helper-hardhat-config.ts`) which contains the parameters required for VRF on test networks like Goerli and Sepolia.

However, you still need to manually replace the `subscriptionId` value with your own. To obtain one, you need to [Create and fund a VRF subscription](https://docs.chain.link/docs/vrf/v2/subscription/ui/).

To have a fully working solution, both [Chainlink VRF](https://docs.chain.link/docs/vrf-contracts/) and [Chainlink Automation](https://docs.chain.link/docs/chainlink-automation/supported-networks/) have to be supported on the network.

## Register Upkeep

To automate the metadata reveal with Chainlink’s Automation network, you need to [register new Upkeeep](https://automation.chain.link/new) by following this [step-by-step guide](https://docs.chain.link/docs/chainlink-automation/register-upkeep/).

## User Interface

An [app](/app) is included in this repo which helps you configure and deploy collections from the browser using MetaMask or WalletConnect. It then generates a shareable collection page to mint, monitor when the next reveal is, and browse through the collection of NFTs.

## Misc

To flatten the contract which is used to open and deploy via Remix IDE:

```bash
yarn flatten
```

## Refs

- [Chainlink Automation Docs](https://docs.chain.link/docs/chainlink-automation/introduction/)
- [Chainlink VRF Docs](https://docs.chain.link/docs/chainlink-vrf/)
- [Designing Effective NFT Launches](https://www.paradigm.xyz/2021/10/a-guide-to-designing-effective-nft-launches)
- [Smart Batched Auctions](https://github.com/FrankieIsLost/smart-batched-auction)


