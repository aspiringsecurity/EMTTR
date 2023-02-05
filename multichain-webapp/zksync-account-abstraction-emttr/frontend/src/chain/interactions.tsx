import { ethers } from "ethers";
import { utils, Wallet, Provider, EIP712Signer, types } from "zksync-web3";

import { MetaMaskInpageProvider } from "@metamask/providers";

import {
    AA_FACTORY_ARTIFACT,
    AA_ARTIFACT,
    LIMITER_PLUGIN_ADDRESS,
    LIMITER_PLUGIN_ARTIFACT,
    AA_FACTORY_ADDRESS,
    GREETER_ADDRESS,
    GREETER_ARTIFACT,
    ZKSYNC_URL_PROVIDER,
} from "../config";

let zkSyncProvider = new Provider(ZKSYNC_URL_PROVIDER);

declare global {
    interface Window {
        ethereum?: MetaMaskInpageProvider;
    }
}

let _ethereum: MetaMaskInpageProvider;
let provider: any;
let signer: any;
let account: any;
let aaFactory: any;
let aa: any;
let limiterPlugin: any;
let greeter: any;

const getSigner = () => {
    if (signer == null) {
        return { err: "Need to sign in first.", signer: null };
    }
    return { err: "", signer };
};

const getAccount = () => {
    if (account == null) {
        return { err: "Need to sign in first.", account: null };
    }
    return { err: "", account };
};

const delay = async () => {
    await new Promise((r) => setTimeout(r, 2000));
};

const init = async (props: { updateState: any }) => {
    const { ethereum } = window;
    if (ethereum != null) {
        const _init = async () => {
            _ethereum = ethereum;
            await ethereum.request({ method: "eth_requestAccounts" });
            const accounts = await ethereum.request({ method: "eth_accounts" });
            account = (accounts as any)[0];
            const _provider = new ethers.providers.Web3Provider(
                ethereum as any,
                "any"
            );
            console.log(_provider);
            await _provider.send("eth_requestAccounts", []);
            provider = _provider;
            signer = provider.getSigner();
            aaFactory = new ethers.Contract(
                AA_FACTORY_ADDRESS,
                AA_FACTORY_ARTIFACT.abi,
                signer
            );

            limiterPlugin = new ethers.Contract(
                LIMITER_PLUGIN_ADDRESS,
                LIMITER_PLUGIN_ARTIFACT.abi,
                signer
            );
            greeter = new ethers.Contract(
                GREETER_ADDRESS,
                GREETER_ARTIFACT.abi,
                signer // dummyWallet
            );
            props.updateState({ account: (accounts as any)[0] });
        };

        await _init();

        return { err: "" };
    }
    return { err: "Need to install Metamask" };
};

const deployNewAA = async (props: { ownerAddr: string }) => {
    console.log("called", { deployNewAA: props });
    const { ownerAddr } = props;

    await delay();
    const salt = ethers.utils.hexZeroPad(
        ethers.utils.hexlify(Math.floor(Math.random() * 332132312)),
        32
    );

    const tx = await aaFactory.deployAccount(salt, ownerAddr);
    await tx.wait();
    // Getting the address of the deployed contract
    const abiCoder = new ethers.utils.AbiCoder();
    const pluginableAccountAddress = utils.create2Address(
        AA_FACTORY_ADDRESS,
        await aaFactory.aaBytecodeHash(),
        salt,
        abiCoder.encode(["address"], [ownerAddr])
    );
    return {
        err: "",
        aaAddr: pluginableAccountAddress,
    };
};

const getAA_Info = async (props: { addr: string }) => {
    console.log("called", { getAA_Info: props });

    aa = new ethers.Contract(props.addr, AA_ARTIFACT.abi, signer);

    const { addr } = props;
    const limit = parseInt(
        (await limiterPlugin.activeLimits(addr, account)).toString()
    );

    console.log({ limit });

    const owner = await aa.owner();
    const balance = `${(await provider.getBalance(addr)).toString()} WEI`;

    if (limit > 0) {
        return {
            err: "",
            owner,
            balance,
            plugins: [
                {
                    name: "Enforced Limit",
                    authority: account,
                    limit: `${limit} WEI`,
                },
            ],
        };
    } else {
        return {
            err: "",
            owner,
            balance,
            plugins: [],
        };
    }
};

const addLimitPlugin = async (props: { aaAddr: string }) => {
    aa = new ethers.Contract(props.aaAddr, AA_ARTIFACT.abi, signer);
    await aa.activatePlugin(LIMITER_PLUGIN_ADDRESS);
    return { err: "" };
};

const createLimitPlugin = async (props: {
    aaAddr: string;
    address: string;
    limit: string;
}) => {
    console.log("called", { createLimitPlugin: props });

    let aaTx = await limiterPlugin.populateTransaction.addWallets([
        {
            spender: props.address,
            amount: props.limit,
        },
    ]);

    const gasLimit = await provider.estimateGas(aaTx); //ethers.BigNumber.from("200000000");
    const gasPrice = await provider.getGasPrice();

    aaTx = {
        ...aaTx,
        from: props.aaAddr,
        gasLimit: gasLimit,
        gasPrice: gasPrice,
        chainId: (await provider.getNetwork()).chainId,
        nonce: await provider.getTransactionCount(props.aaAddr),
        type: 113,
        customData: {
            ergsPerPubdata: utils.DEFAULT_ERGS_PER_PUBDATA_LIMIT,
        } as types.Eip712Meta,
        value: ethers.BigNumber.from(0),
    };

    const signedTxHash = EIP712Signer.getSignedDigest(aaTx);

    const signature = await _ethereum?.request({
        method: "eth_sign",
        params: [account, signedTxHash],
    });

    aaTx.customData = {
        ...aaTx.customData,
        customSignature: signature,
    };

    console.log({ aaTx });

    const sentTx = await zkSyncProvider.sendTransaction(utils.serialize(aaTx));
    await sentTx.wait();

    console.log({ sentTx });

    return { err: "" };
};

const deactivateLimitPlugin = async (props: {
    aaAddr: string;
    authority: string;
    limit: string;
}) => {
    console.log("called", { createLimitPlugin: props });

    let aaTx = await limiterPlugin.populateTransaction.removeWallets([
        props.authority,
    ]);

    const gasLimit = await provider.estimateGas(aaTx); //ethers.BigNumber.from("20000000");
    const gasPrice = await provider.getGasPrice();

    aaTx = {
        ...aaTx,
        from: props.aaAddr,
        gasLimit: gasLimit,
        gasPrice: gasPrice,
        chainId: (await provider.getNetwork()).chainId,
        nonce: await provider.getTransactionCount(props.aaAddr),
        type: 113,
        customData: {
            ergsPerPubdata: utils.DEFAULT_ERGS_PER_PUBDATA_LIMIT,
        } as types.Eip712Meta,
        value: ethers.BigNumber.from(0),
    };

    const signedTxHash = EIP712Signer.getSignedDigest(aaTx);

    const signature = await _ethereum?.request({
        method: "eth_sign",
        params: [account, signedTxHash],
    });

    aaTx.customData = {
        ...aaTx.customData,
        customSignature: signature,
    };

    console.log({ aaTx });

    const sentTx = await zkSyncProvider.sendTransaction(utils.serialize(aaTx));
    await sentTx.wait();

    console.log({ sentTx });

    return { err: "" };
};

const setGreeting = async (props: {
    aaAddr: string;
    message: string;
    amount: string;
}) => {
    console.log("called", { greet: props });

    try {
        let aaTx = await greeter.populateTransaction.setGreeting(
            props.message,
            { value: ethers.BigNumber.from(props.amount) }
        );

        const gasLimit = await zkSyncProvider.estimateGas(aaTx);
        const gasPrice = await zkSyncProvider.getGasPrice();

        aaTx = {
            ...aaTx,
            from: props.aaAddr,
            gasLimit: gasLimit,
            gasPrice: gasPrice,
            chainId: (await zkSyncProvider.getNetwork()).chainId,
            nonce: await zkSyncProvider.getTransactionCount(props.aaAddr),
            type: 113,
            customData: {
                ergsPerPubdata: utils.DEFAULT_ERGS_PER_PUBDATA_LIMIT,
            } as types.Eip712Meta,
            value: ethers.BigNumber.from(props.amount),
        };

        const signedTxHash = EIP712Signer.getSignedDigest(aaTx);

        const signature = await _ethereum?.request({
            method: "eth_sign",
            params: [account, signedTxHash],
        });

        aaTx.customData = {
            ...aaTx.customData,
            customSignature: signature,
        };

        console.log(aaTx);

        const sentTx = await zkSyncProvider.sendTransaction(
            utils.serialize(aaTx)
        );
        await sentTx.wait();

        console.log({ sentTx });

        return { err: "" };
    } catch (err) {
        console.log(err, (err as any).body);
        return { err: "Limit Plugin Validation failed" };
    }
};

const retrieveGreeting = async () => {
    const greeting = await greeter.greet();
    console.log(greeting);
    return { err: "", greeting };
};

const hasLimitPlugin = async (props: { aaAddr: string }) => {
    aa = new ethers.Contract(props.aaAddr, AA_ARTIFACT.abi, signer);
    try {
        await aa.activePlugins(0);
        return { err: "", hasIt: true };
    } catch {
        return { err: "", hasIt: false };
    }
};

export {
    init,
    deployNewAA,
    getAA_Info,
    getSigner,
    getAccount,
    createLimitPlugin,
    deactivateLimitPlugin,
    setGreeting,
    retrieveGreeting,
    addLimitPlugin,
    hasLimitPlugin,
};
