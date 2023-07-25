import {describe, expect, test} from "@jest/globals";
import { ethers } from "ethers";
import {
    getHub,
    getSpoke,
    loadDeployedAddresses as getDeployedAddresses,
    getWallet,
    getChain,
    wait
} from "./utils"
import {
    getStatus
} from "./getStatus"
import {
    ERC20Mock__factory,
    ITokenBridge__factory
} from "./ethers-contracts"
import {
    tryNativeToUint8Array,
    CHAIN_ID_TO_NAME
} from "@certusone/wormhole-sdk"

const hubChain = 6;
const spokeChain = 14;
const myWallet = getWallet(spokeChain);
const spokeToken = ERC20Mock__factory.connect(getDeployedAddresses().erc20s[spokeChain][0], myWallet);
const spoke = getSpoke(spokeChain);
const hub = getHub();

async function deposit(amount: ethers.BigNumberish) {
    const spokeTokenOriginalBalance = await spokeToken.balanceOf(myWallet.address);
    const cost = await spoke.quoteDeposit();
    console.log(`Cost of deposit: ${ethers.utils.formatEther(cost)} testnet ${CHAIN_ID_TO_NAME[spokeChain]}`);

    const approveTx = await spokeToken.approve(spoke.address, amount).then(wait);
    console.log(`Depositing ${ethers.utils.formatEther(amount)} of spoke token`)

    const tx = await spoke.deposit(spokeToken.address, amount, {value: cost});
    console.log(`Transaction hash: ${tx.hash}`);
    await tx.wait();

    await new Promise(resolve => setTimeout(resolve, 1000*15));

    const spokeTokenBalance = await spokeToken.balanceOf(myWallet.address);
    expect(spokeTokenOriginalBalance.sub(spokeTokenBalance).toString()).toBe(amount.toString());
}

async function withdraw(amount: ethers.BigNumberish) {
    const spokeTokenOriginalBalance = await spokeToken.balanceOf(myWallet.address);
    const cost = await spoke.quoteWithdraw();
    console.log(`Cost of withdraw: ${ethers.utils.formatEther(cost)} testnet ${CHAIN_ID_TO_NAME[spokeChain]}`);

    console.log(`Withdrawing ${ethers.utils.formatEther(amount)} of spoke token`)

    const tx = await spoke.withdraw(spokeToken.address, amount, {value: cost});
    console.log(`Transaction hash: ${tx.hash}`);
    await tx.wait();

    await new Promise(resolve => setTimeout(resolve, 1000*30));

    const spokeTokenBalance = await spokeToken.balanceOf(myWallet.address);
    expect(spokeTokenBalance.sub(spokeTokenOriginalBalance).toString()).toBe(amount.toString());
}

async function repay(amount: ethers.BigNumberish) {
    const spokeTokenOriginalBalance = await spokeToken.balanceOf(myWallet.address);
    const cost = await spoke.quoteRepay();
    console.log(`Cost of repay: ${ethers.utils.formatEther(cost)} testnet ${CHAIN_ID_TO_NAME[spokeChain]}`);

    const approveTx = await spokeToken.approve(spoke.address, amount).then(wait);
    console.log(`Repaying ${ethers.utils.formatEther(amount)} of spoke token`)

    const tx = await spoke.deposit(spokeToken.address, amount, {value: cost});
    console.log(`Transaction hash: ${tx.hash}`);
    await tx.wait();

    await new Promise(resolve => setTimeout(resolve, 1000*15));

    const spokeTokenBalance = await spokeToken.balanceOf(myWallet.address);
    expect(spokeTokenOriginalBalance.sub(spokeTokenBalance).toString()).toBe(amount.toString());
}

async function borrow(amount: ethers.BigNumberish) {
    const spokeTokenOriginalBalance = await spokeToken.balanceOf(myWallet.address);
    const cost = await spoke.quoteBorrow();
    console.log(`Cost of borrow: ${ethers.utils.formatEther(cost)} testnet ${CHAIN_ID_TO_NAME[spokeChain]}`);

    console.log(`Borrowing ${ethers.utils.formatEther(amount)} of spoke token`)

    const tx = await spoke.borrow(spokeToken.address, amount, {value: cost});
    console.log(`Transaction hash: ${tx.hash}`);
    await tx.wait();

    await new Promise(resolve => setTimeout(resolve, 1000*30));

    const spokeTokenBalance = await spokeToken.balanceOf(myWallet.address);
    expect(spokeTokenBalance.sub(spokeTokenOriginalBalance).toString()).toBe(amount.toString());
}

describe("Cross Chain Borrow Lend Tests on Testnet", () => {
    test("Tests a deposit", async () => {
        // Token Bridge can only deal with 8 decimal places
        // So we send a multiple of 10^10, since this MockToken has 18 decimal places
        const arbitraryTokenAmount = ethers.BigNumber.from((new Date().getTime()) % (10 ** 7)).mul(10**10);
        
        await deposit(arbitraryTokenAmount);

    }, 60*1000) // timeout

    test("Tests a deposit and withdraw", async () => {
        // Token Bridge can only deal with 8 decimal places
        // So we send a multiple of 10^10, since this MockToken has 18 decimal places
        const arbitraryTokenAmount = ethers.BigNumber.from((new Date().getTime()) % (10 ** 7)).mul(10**10);
        
        await deposit(arbitraryTokenAmount);
        await withdraw(arbitraryTokenAmount);

    }, 90*1000) // timeout

    test("Tests a deposit, withdraw, borrow, repay", async () => {
        // Token Bridge can only deal with 8 decimal places
        // So we send a multiple of 10^10, since this MockToken has 18 decimal places
        const arbitraryTokenAmount = ethers.BigNumber.from((new Date().getTime()) % (10 ** 7)).mul(10**10);
        
        await deposit(arbitraryTokenAmount);
        await borrow(arbitraryTokenAmount);
        await repay(arbitraryTokenAmount);
        await withdraw(arbitraryTokenAmount);

    }, 180*1000) // timeout
})