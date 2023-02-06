import { HardhatUserConfig, task } from "hardhat/config";
import "@nomicfoundation/hardhat-toolbox";
require('dotenv').config({path:__dirname+'/.env'})

const { ALCHEMY_API_KEY, PRIVATE_KEY } = process.env;

const config: HardhatUserConfig = {
  paths: { tests: "tests" },
  solidity: "0.8.17",
  networks: {
    localhost: {
      url: "http://127.0.0.1:8545"
    },
    goerli: {
      url: `https://eth-goerli.alchemyapi.io/v2/${ALCHEMY_API_KEY}`,
      accounts: [`${PRIVATE_KEY}`]
    },
    altlayer_test: {
      url: 'https://devnet-rpc.altlayer.io/',
      accounts: [`${PRIVATE_KEY}`],
      chainId: 9990
    }
  }
};

export default config;

task("accounts", "Prints the list of accounts", async (taskArgs, hre) => {
  const accounts = await hre.ethers.getSigners();

  for (const account of accounts) {
    console.log(account.address);
  }
});