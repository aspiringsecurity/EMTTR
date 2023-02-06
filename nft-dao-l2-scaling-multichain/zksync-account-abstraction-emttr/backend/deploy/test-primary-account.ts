import { utils, Wallet, Provider, EIP712Signer, types } from "zksync-web3";
import * as ethers from "ethers";
import { HardhatRuntimeEnvironment } from "hardhat/types";

import sanitizedConfig from "../config";

// An example of a deploy script deploys and calls a simple contract.
export default async function (hre: HardhatRuntimeEnvironment) {
    console.log(hre.config.zkSyncDeploy.zkSyncNetwork);
    const provider = new Provider(hre.config.zkSyncDeploy.zkSyncNetwork);
    const wallet = new Wallet(sanitizedConfig.PRIVATE_KEY).connect(provider);
    const factoryArtifact = await hre.artifacts.readArtifact("AAFactory");
    const accountArtifact = await hre.artifacts.readArtifact(
        "PluginableAccount"
    );

    const accountInstance = new ethers.Contract(
        sanitizedConfig.ACCOUNT_ADDRESS,
        accountArtifact.abi,
        wallet
    );

    // The two owners of the multisig
    const owner = new Wallet(sanitizedConfig.PRIVATE_KEY);
    const spender = new Wallet(sanitizedConfig.ALTERNATE_PRIVATE_KEY);

    // Getting the address of the deployed contract
    const abiCoder = new ethers.utils.AbiCoder();
    const pluginableAccountAddress = sanitizedConfig.ACCOUNT_ADDRESS;
    console.log(`Account deployed on address ${pluginableAccountAddress}`);

    // await (
    //     await wallet.sendTransaction({
    //         to: pluginableAccountAddress,
    //         // You can increase the amount of ETH sent to the multisig
    //         value: ethers.utils.parseEther("0.01"),
    //     })
    // ).wait();

    let aaTx = await accountInstance.populateTransaction.activatePlugin(
        sanitizedConfig.PLUGIN_ADDRESS
    );

    const gasLimit = ethers.BigNumber.from("2000000");
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

    console.log(aaTx);
    const signedTxHash = EIP712Signer.getSignedDigest(aaTx);

    const signature = ethers.utils.joinSignature(
        owner._signingKey().signDigest(signedTxHash)
    );

    aaTx.customData = {
        ...aaTx.customData,
        customSignature: signature,
    };

    console.log(
        `The account's nonce before the first tx is ${await provider.getTransactionCount(
            pluginableAccountAddress
        )}`
    );
    const sentTx = await provider.sendTransaction(utils.serialize(aaTx));
    await sentTx.wait();

    // Checking that the nonce for the account has increased
    console.log(
        `The account's nonce after the first tx is ${await provider.getTransactionCount(
            pluginableAccountAddress
        )}`
    );
}
