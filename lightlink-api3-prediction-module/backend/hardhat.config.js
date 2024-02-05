require("dotenv").config();
require('@nomiclabs/hardhat-waffle');
require("hardhat-deploy");

const PK_DEPLOYER = process.env.PK_DEPLOYER;

module.exports = {
  solidity: {
    version: "0.8.18",
    settings: {
      optimizer: {
        enabled: true,
        runs: 100,
      },
    },
  },
  networks: {
    hardhat: {
      chainId: 31337,
    },
    mumbai: {
      chainId: 80001,
      url: "https://polygon-testnet.public.blastapi.io",
      accounts: [PK_DEPLOYER],
    },
    goerli: {
      chainId: 5,
      url: "https://eth-goerli.g.alchemy.com/v2/Py5g-Lx7Yk4QzMHNLZ8SGMcj5ovkdZLL",
      accounts: [PK_DEPLOYER],
    },
  },
  namedAccounts: {
    deployer: {
      default: 0,
    },
  },
};
