import { utils, Wallet, Provider, EIP712Signer, types } from "zksync-web3";
import * as ethers from "ethers";
import { HardhatRuntimeEnvironment } from "hardhat/types";

import sanitizedConfig from "../config";

// An example of a deploy script deploys and calls a simple contract.
export default async function (hre: HardhatRuntimeEnvironment) {
    const provider = new Provider(hre.config.zkSyncDeploy.zkSyncNetwork);
    const wallet = new Wallet(sanitizedConfig.PRIVATE_KEY).connect(provider);
    const factoryArtifact = await hre.artifacts.readArtifact("AAFactory");
    const greeterArtifact = await hre.artifacts.readArtifact("Greeter");

    const greeterInstance = new ethers.Contract(sanitizedConfig.GREETER_ADDRESS, greeterArtifact.abi, wallet);

    const greetingFromContract = await greeterInstance.greet();
    console.log(`Contract initially greets us with ${greetingFromContract}!`);
    const newGreeting = "Wazaaaaaaaaaa";


    // The two owners of the multisig
    const owner = new Wallet(sanitizedConfig.PRIVATE_KEY);
    const spender = new Wallet(sanitizedConfig.ALTERNATE_PRIVATE_KEY);


    // For the simplicity of the tutorial, we will use zero hash as salt
    const salt = ethers.constants.HashZero;


    // Getting the address of the deployed contract
    const abiCoder = new ethers.utils.AbiCoder();
    const pluginableAccountAddress = sanitizedConfig.ACCOUNT_ADDRESS;
    console.log(`Account deployed on address ${pluginableAccountAddress}`);

    let aaTx = await greeterInstance.populateTransaction.setGreeting(newGreeting);

    const gasLimit = await provider.estimateGas(aaTx);
    const gasPrice = await provider.getGasPrice();

    aaTx = {
        ...aaTx,
        from: pluginableAccountAddress,
        gasLimit: gasLimit,
        gasPrice: gasPrice,
        chainId: (await provider.getNetwork()).chainId,
        nonce: await provider.getTransactionCount(pluginableAccountAddress),
        type: 113,
        customData: {
            ergsPerPubdata: utils.DEFAULT_ERGS_PER_PUBDATA_LIMIT,
        } as types.Eip712Meta,
        value: ethers.BigNumber.from(0),
    };
    const signedTxHash = EIP712Signer.getSignedDigest(aaTx);

    const signature = ethers.utils.joinSignature(spender._signingKey().signDigest(signedTxHash));

    aaTx.customData = {
        ...aaTx.customData,
        customSignature: signature,
    };

    console.log(`The multisig's nonce before the first tx is ${await provider.getTransactionCount(pluginableAccountAddress)}`);
    const sentTx = await provider.sendTransaction(utils.serialize(aaTx));
    await sentTx.wait();

    // Checking that the nonce for the account has increased
    console.log(`The multisig's nonce after the first tx is ${await provider.getTransactionCount(pluginableAccountAddress)}`);
}