import { utils, Wallet } from "zksync-web3";
import * as ethers from "ethers";
import { HardhatRuntimeEnvironment } from "hardhat/types";
import { Deployer } from "@matterlabs/hardhat-zksync-deploy";
import hre from "hardhat";
import sanitizedConfig from "../config";

// An example of a deploy script that will deploy and call a simple contract.
const main = async (hre: HardhatRuntimeEnvironment) => {
    console.log(`Running deploy script for the Plugin contract`);

    // Initialize the wallet.
    const wallet = new Wallet(sanitizedConfig.PRIVATE_KEY);

    // Create deployer object and load the artifact of the contract we want to deploy.
    const deployer = new Deployer(hre, wallet);
    const artifact = await deployer.loadArtifact("Plugin");

    // // Deposit some funds to L2 in order to be able to perform L2 transactions.
    // const depositAmount = ethers.utils.parseEther("0.05");
    // const depositHandle = await deployer.zkWallet.deposit({
    //     to: deployer.zkWallet.address,
    //     token: utils.ETH_ADDRESS,
    //     amount: depositAmount,
    // });
    // // Wait until the deposit is processed on zkSync
    // await depositHandle.wait();

    // console.log(`Deposited ETH`);

    // Deploy this contract. The returned object will be of a `Contract` type, similarly to ones in `ethers`.
    // `greeting` is an argument for contract constructor.
    const pluginContract = await deployer.deploy(artifact);

    // Show the contract info.
    const contractAddress = pluginContract.address;
    console.log(`${artifact.contractName} was deployed to ${contractAddress}`);
};

main(hre).then(() => process.exit(0));
