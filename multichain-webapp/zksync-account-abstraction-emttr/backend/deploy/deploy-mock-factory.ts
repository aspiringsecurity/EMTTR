import { utils, Wallet } from "zksync-web3";
import * as ethers from "ethers";
import { HardhatRuntimeEnvironment } from "hardhat/types";
import { Deployer } from "@matterlabs/hardhat-zksync-deploy";
const hre = require("hardhat");

import sanitizedConfig from "../config";

const main = async (hre: HardhatRuntimeEnvironment) => {
    const wallet = new Wallet(sanitizedConfig.PRIVATE_KEY);
    const deployer = new Deployer(hre, wallet);
    const factoryArtifact = await deployer.loadArtifact("AAFactory");
    const aaArtifact = await deployer.loadArtifact("MockAccount");

    // // Deposit some funds to L2 in order to be able to perform L2 transactions.
    // // You can remove the depositing step if the `wallet` has enough funds on zkSync
    // const depositAmount = ethers.utils.parseEther("0.3");
    // const depositHandle = await deployer.zkWallet.deposit({
    //     to: deployer.zkWallet.address,
    //     token: utils.ETH_ADDRESS,
    //     amount: depositAmount,
    // });
    // await depositHandle.wait();

    // Getting the bytecodeHash of the account
    const bytecodeHash = utils.hashBytecode(aaArtifact.bytecode);

    const factory = await deployer.deploy(
        factoryArtifact,
        [bytecodeHash],
        undefined,
        [
            // Since the factory requires the code of the multisig to be available,
            // we should pass it here as well.
            aaArtifact.bytecode,
        ]
    );

    console.log(`Mock AA factory address: ${factory.address}`);
};

main(hre)
    .then(() => {
        console.log(123);
        process.exit(0);
    })
    .catch((error) => {
        console.error(error);
        process.exit(1);
    });
