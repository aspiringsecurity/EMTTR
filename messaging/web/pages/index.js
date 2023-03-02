//index.js

import { useState, useEffect } from "react";
import cn from "classnames";
import { ethers, getDefaultProvider, Contract, AddressZero, Wallet, BigNumber } from "ethers";
import { defaultAbiCoder } from "ethers/lib/utils";
import Loader from "react-loader-spinner";
import { ChainList } from "../utils/ChainList.js";
import { TextInput } from "./components/TextInput.js";
import { checkIfWalletIsConnected } from "../utils/checkIfWalletIsConnected";
import { AxelarQueryAPI } from "@axelar-network/axelarjs-sdk";
import { ChainCard } from "./components/ChainComponents";
import NetworkInfo from "../info/local.json";
import ContractABI from "../abi/CallContractWithToken.json";
import IERC20 from "../abi/IERC20.json";
import Gateway from "../abi/IAxelarGateway.json";

const axelarApi = new AxelarQueryAPI({ environment: "testnet" });

const app = () => {
    const [miningStatus, setMiningStatus] = useState(null);
    const [loadingState, setLoadingState] = useState(0);
    const [needInput, setNeedInput] = useState(true);
    const [txError, setTxError] = useState(null);
    const [currentAccount, setCurrentAccount] = useState("");
    const [srcChain, setSrcChain] = useState(ChainList[0]);
    const [destChain, setDestChain] = useState(ChainList[1]);
    const [destAddresses, setDestAddresses] = useState([]);
    const [amountToSend, setAmountToSend] = useState(0);
    const [environment, setEnvironment] = useState("local");
    const [devChainId, setDevChainId] = useState(
        NetworkInfo.find((network) => network.name.toLowerCase() === srcChain.name.toLowerCase())
            .chainId
    );
    const [destBalances, setDestBalances] = useState(null);

    useEffect(() => {
        checkIfWalletIsConnected(setCurrentAccount);
    }, []);

    const executeCallContractWithToken = async () => {
        setNeedInput(false);
        const source = NetworkInfo.find(
            (chain) => chain.name.toLowerCase() === srcChain.name.toLowerCase()
        );
        const destination = NetworkInfo.find(
            (chain) => chain.name.toLowerCase() === destChain.name.toLowerCase()
        );
        const amount = Math.floor(parseFloat(amountToSend)) * 1e6 || 10e6;

        let wallet;

        const mnemonic = process.env.NEXT_PUBLIC_EVM_MNEMONIC;
        const private_key = process.env.NEXT_PUBLIC_EVM_PRIVATE_KEY;

        if (mnemonic !== null && mnemonic.length > 0) {
            wallet = Wallet.fromMnemonic(mnemonic);
        } else if (private_key !== null && private_key.length > 0) {
            wallet = new Wallet(private_key);
        }

        for (const chain of [source, destination]) {
            chain.wallet = wallet.connect(getDefaultProvider(chain.rpc));
            chain.contract = new Contract(
                chain.contractCallWithToken,
                ContractABI.abi,
                chain.wallet
            );
            chain.gatewayContract = new Contract(chain.gateway, Gateway.abi, chain.wallet);
            const usdcAddress = await chain.gatewayContract.tokenAddresses("aUSDC");
            console.log("aUSDC address for chain", chain.rpc, usdcAddress);
            chain.usdc = new Contract(usdcAddress, IERC20.abi, chain.wallet);
        }

        async function print() {
            for (const account of destAddresses) {
                console.log(
                    `${wallet.address} has ${
                        (await source.usdc.balanceOf(account)) / 1e6
                    } aUSDC on ${source.name}`
                );
                console.log(
                    `${account} has ${(await destination.usdc.balanceOf(account)) / 1e6} aUSDC on ${
                        destination.name
                    }`
                );
            }
        }

        function sleep(ms) {
            return new Promise((resolve) => {
                setTimeout(() => {
                    resolve();
                }, ms);
            });
        }

        const gasLimit = 3e6;
        let gasPrice;

        try {
            gasPrice =
                environment === "local"
                    ? gasLimit
                    : await axelarApi.estimateGasFee(
                          source.name.toLowerCase(),
                          destination.name.toLowerCase(),
                          "USDC"
                      );
        } catch (e) {
            gasPrice = gasLimit;
        }

        setMiningStatus(0);
        setLoadingState(0);

        const balance = BigInt(await destination.usdc.balanceOf(destAddresses[0]));
        await (await source.usdc.approve(source.contract.address, amount)).wait();
        const tx = await (
            await source.contract.methodOnSrcChain(
                destination.name,
                destination.contractCallWithToken,
                defaultAbiCoder.encode(["address[]"], [destAddresses]),
                "aUSDC",
                amount,
                { value: BigNumber.from(gasPrice) }
            )
        ).wait();
        console.log("tx", tx);
        while (BigInt(await destination.usdc.balanceOf(destAddresses[0])) == balance) {
            await sleep(2000);
        }
        setLoadingState(1);
        console.log("--- After ---");
        await print();
        setDestBalances(await getBalances(destChain.name, destAddresses));
    };

    return (
        <div className="flex flex-col items-center pt-32 bg-[#0B132B] text-[#d3d3d3] min-h-screen">
            <h2 className="mt-12 mb-20 text-3xl font-bold">
                Sample ContractCallWithToken: Let's airdrop!
            </h2>
            {needInput && (
                <div className="w-4/6 shadow-xl bg-base-100">
                    <div className="grid grid-cols-2 gap-10 ">
                        {" "}
                        {ChainCard(srcChain, (option) => {
                            setSrcChain(option);
                            setDevChainId(
                                NetworkInfo.find(
                                    (network) =>
                                        network.name.toLowerCase() === option.name.toLowerCase()
                                )?.chainId
                            );
                        })}
                        {ChainCard(destChain, (option) => setDestChain(option))}
                    </div>
                    <div className="flex flex-col items-center justify-center mb-10 text-2xl font-bold">
                        <div className="form-control">
                            <label className="label">
                                <span className="label-text">Enter aUSDC amount</span>
                            </label>
                            <label className="input-group">
                                <input
                                    type="text"
                                    placeholder="5"
                                    className="input input-bordered"
                                    onChange={(e) => setAmountToSend(e.target.value)}
                                />
                                <span>aUSDC</span>
                            </label>
                        </div>
                    </div>
                    <div className="flex flex-col items-center justify-center mb-10 text-2xl font-bold">
                        <TextInput
                            className={"w-1/2"}
                            cb={(addr) => setDestAddresses([...destAddresses, addr])}
                        />
                    </div>
                    <div className="flex flex-row items-center justify-center gap-2 mb-10 text-2xl font-bold">
                        {destAddresses?.map((addr) => (
                            <div key={`dest-addr-${addr}`} className="badge badge-primary">
                                {addr.slice(0, 5) + "..." + addr.slice(35)}
                            </div>
                        ))}
                    </div>
                    <div className="flex flex-row items-center justify-center gap-2 mb-10 text-2xl font-bold">
                        <button
                            disabled={destAddresses.length === 0}
                            className={cn(
                                "px-12 py-3 mb-10 btn-lg font-bold transition duration-500 ease-in-out btn btn-success hover:scale-105",
                                {
                                    "btn-disabled": destAddresses.length === 0
                                }
                            )}
                            onClick={executeCallContractWithToken}
                        >
                            Execute CallContractWithToken
                        </button>
                    </div>
                </div>
            )}

            {loadingState === 0 ? (
                miningStatus === 0 ? (
                    txError === null ? (
                        <div className="flex flex-col items-center justify-center">
                            <div className="text-lg font-bold">Processing your transaction</div>
                            <Loader
                                className="flex items-center justify-center pt-12"
                                type="TailSpin"
                                color="#d3d3d3"
                                height={40}
                                width={40}
                            />
                        </div>
                    ) : (
                        <div className="text-lg font-semibold text-red-600">{txError}</div>
                    )
                ) : (
                    <div></div>
                )
            ) : (
                <div className="flex flex-col justify-center items-center h-60 w-60 rounded-lg shadow-2xl shadow-[#6FFFE9] hover:scale-105 transition duration-500 ease-in-out">
                    <div>Transfers Complete!</div>
                    <div>Updated Balances on {destChain.name}</div>
                    <div>
                        {destBalances &&
                            destBalances.map((destBal) => {
                                return (
                                    <div
                                        key={`dest-balance-${destBal.address}`}
                                        className="flex flow-row"
                                    >
                                        <span>0x...{destBal.address.slice(35)}: </span>
                                        <span>{destBal.balance}</span>
                                    </div>
                                );
                            })}
                    </div>
                </div>
            )}
        </div>
    );
};

export async function getBalances(chainName, addresses) {
    const destination = NetworkInfo.find(
        (chain) => chain.name.toLowerCase() === chainName.toLowerCase()
    );
    const balances = [];

    for (let i = 0; i < addresses.length; i++) {
        const addr = addresses[i];
        balances.push({
            chain: chainName,
            address: addr,
            balance: ethers.utils.formatUnits(await destination.usdc.balanceOf(addr), 6)
        });
    }
    return balances;
}

export default app;
