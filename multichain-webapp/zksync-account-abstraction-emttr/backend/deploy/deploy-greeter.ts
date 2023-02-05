import { utils, Wallet } from "zksync-web3";
import * as ethers from "ethers";
import { HardhatRuntimeEnvironment } from "hardhat/types";
import { Deployer } from "@matterlabs/hardhat-zksync-deploy";
import hre from "hardhat";
import sanitizedConfig from "../config";

// An example of a deploy script that will deploy and call a simple contract.
const main = async (hre: HardhatRuntimeEnvironment) => {
    console.log(`Running deploy script for the Greeter contract`);

    // Initialize the wallet.
    const wallet = new Wallet(sanitizedConfig.PRIVATE_KEY);

    // Create deployer object and load the artifact of the contract we want to deploy.
    const deployer = new Deployer(hre, wallet);
    const artifact = await deployer.loadArtifact("Greeter");

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
    const greeting = "Hello there!";
    const greeterContract = await deployer.deploy(artifact, []);

    // Show the contract info.
    const contractAddress = greeterContract.address;
    console.log(`${artifact.contractName} was deployed to ${contractAddress}`);

    // Call the deployed contract.
    const greetingFromContract = await greeterContract.greet();
    if (greetingFromContract == greeting) {
        console.log(`Contract greets us with ${greeting}!`);
    } else {
        console.error(
            `Contract said something unexpected: ${greetingFromContract}`
        );
    }

    // Edit the greeting of the contract
    const newGreeting = "General Kenobi";
    const setNewGreetingHandle = await greeterContract.setGreeting(newGreeting);
    await setNewGreetingHandle.wait();

    const newGreetingFromContract = await greeterContract.greet();
    if (newGreetingFromContract == newGreeting) {
        console.log(`Contract greets us with ${newGreeting}!`);
    } else {
        console.error(
            `Contract said something unexpected: ${newGreetingFromContract}`
        );
    }
};

main(hre).then(() => process.exit(0));
