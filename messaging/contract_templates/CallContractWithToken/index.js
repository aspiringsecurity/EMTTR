'use strict';

const { getDefaultProvider, Contract, utils: { defaultAbiCoder } } = require('ethers');
const { utils: { deployContract }} = require('@axelar-network/axelar-local-dev');

const ContractCallWithToken = require('../../build/CallContractWithToken.json');
const Gateway = require('../../build/IAxelarGateway.json');
const IERC20 = require('../../build/IERC20.json');

async function deploy(chain, wallet) {
    console.log(`Deploying ContractCallWithToken for ${chain.name}.`);
    const contract = await deployContract(wallet, ContractCallWithToken, [chain.gateway, chain.gasReceiver]);
    chain.contractCallWithToken = contract.address;
    console.log(`Deployed ContractCallWithToken for ${chain.name} at ${chain.contractCallWithToken}.`);
}


async function test(chains, wallet, options) {
    const args = options.args || [];
    const getGasPrice = options.getGasPrice;
    const source = chains.find(chain => chain.name == (args[0] || 'Avalanche'));
    const destination = chains.find(chain =>chain.name == (args[1] || 'Fantom'));
    const amount = Math.floor(parseFloat(args[2]) * 1e6) || 10e6;
    const accounts = args.slice(3);
    if(accounts.length == 0)
        accounts.push(wallet.address);
    for(const chain of [source, destination]) {
        const provider = getDefaultProvider(chain.rpc);
        chain.wallet = wallet.connect(provider);
        chain.contract = new Contract(chain.contractCallWithToken, ContractCallWithToken.abi, chain.wallet);
        chain.gateway = new Contract(chain.gateway, Gateway.abi, chain.wallet);
        const usdcAddress = await chain.gateway.tokenAddresses('aUSDC');
        console.log("aUSDC address for chain", chain.rpc, usdcAddress)
        chain.usdc = new Contract(usdcAddress, IERC20.abi, chain.wallet);
    }

    async function print() {
        console.log(`${wallet.address} has ${await source.usdc.balanceOf(wallet.address)/1e6} aUSDC on ${source.name}`)
        for(const account of accounts) {
            console.log(`${account} has ${await destination.usdc.balanceOf(account)/1e6} aUSDC on ${destination.name}`)
        }
    }
    function sleep(ms) {
        return new Promise((resolve)=> {
            setTimeout(() => {resolve()}, ms);
        })
    }

    console.log('--- Initially ---');
    await print();

    let gasFee;
    try {
        gasFee = await getGasPrice(source.name.toLowerCase(), destination.name.toLowerCase(), "USDC");
    } catch (e) {
        gasFee = 1;
    }

    const balance = BigInt(await destination.usdc.balanceOf(accounts[0]));
    await (await source.usdc.approve(
        source.contract.address,
        amount,
    )).wait();
    await (await source.contract.methodOnSrcChain(
        destination.name,
        destination.contractCallWithToken,
        defaultAbiCoder.encode(["address[]"], [accounts]),
        'aUSDC',
        amount,
        {value: BigInt(gasFee)}
    )).wait();
    while(BigInt(await destination.usdc.balanceOf(accounts[0])) == balance) {
        await sleep(2000);
    }

    console.log('--- After ---');
    await print();
}

module.exports = {
    deploy,
    test,
}
