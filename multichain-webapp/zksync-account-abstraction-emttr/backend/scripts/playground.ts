import * as zksync from "zksync-web3";
import * as ethers from "ethers";

import sanitizedConfig from '../config';

// Currently, only one environment is supported.
const syncProvider = new zksync.Provider("https://zksync2-testnet.zksync.dev");
const ethProvider = new ethers.providers.AlchemyProvider("goerli", sanitizedConfig.API_KEY);

// Derive zksync.Wallet from ethereum private key.
// zkSync's wallets support all of the methods of ethers' wallets.
// Also, both providers are optional and can be connected to later via `connect` and `connectToL1`.
const syncWallet = new zksync.Wallet(sanitizedConfig.ALTERNATE_PRIVATE_KEY, syncProvider, ethProvider);

const depositFunction = async (amount: string): Promise<void> => {
    const balance = await ethProvider.getBalance(syncWallet.address);
    const balanceInEth = ethers.utils.formatEther(balance);
    console.log(`Initial Goerli balance: ${balanceInEth} ETH`)

    const deposit = await syncWallet.deposit({
        token: zksync.utils.ETH_ADDRESS,
        amount: ethers.utils.parseEther(amount),
    });

    // Await processing of the deposit on L1
    const ethereumTxReceipt = await deposit.waitL1Commit();

    // Await processing the deposit on zkSync
    const depositReceipt = await deposit.wait();

    // Retreiving the current (committed) balance of an account
    const committedEthBalance = await syncWallet.getBalance(zksync.utils.ETH_ADDRESS);

    // Retrieving the balance of an account in the last finalized block zkSync.md#confirmations-and-finality
    const finalizedEthBalance = await syncWallet.getBalance(zksync.utils.ETH_ADDRESS, "finalized");

    console.log(`Finalized ETH balance ${finalizedEthBalance.toString()}`);    
}

const withdrawFunction = async (amount: string): Promise<void> => {
    const withdrawL2 = await syncWallet.withdraw({
        token: zksync.utils.ETH_ADDRESS,
        amount: ethers.utils.parseEther(amount),
    });

    // Await processing the withdraw on zkSync
    const withdrawReceipt = await withdrawL2.waitFinalize();
    console.log(`Withdraw receipt:\n${withdrawReceipt}`);

    // Retreiving the current (committed) balance of an account
    const committedEthBalance = await syncWallet.getBalance(zksync.utils.ETH_ADDRESS);

    // Retrieving the balance of an account in the last finalized block zkSync.md#confirmations-and-finality
    const finalizedEthBalance = await syncWallet.getBalance(zksync.utils.ETH_ADDRESS, "finalized");
    console.log(`Finalized balance: ${finalizedEthBalance.toString()}`);

}

const main = async (): Promise<void> => {
    await depositFunction('0.01');

    // await withdrawFunction('0.15');
}

main();