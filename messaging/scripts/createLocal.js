require("dotenv").config();
const {
    createAndExport,
    utils: { setJSON, deployContract }
} = require("@axelar-network/axelar-local-dev");
const {
    Wallet,
    utils: { keccak256, defaultAbiCoder }
} = require("ethers");
const fs = require("fs");

const ConstAddressDeployer = require("axelar-utils-solidity/dist/ConstAddressDeployer.json");

(async () => {
    const mnemonic = process.env.EVM_MNEMONIC;
    const private_key = process.env.EVM_PRIVATE_KEY;
    
    let wallet;
    if (!!mnemonic) {
        wallet = Wallet.fromMnemonic(mnemonic);
    } else if (!!private_key) {
        wallet = new Wallet(private_key);
    } else {
        throw new Error("must provide either EVM_MNEMONIC or EVM_PRIVATE_KEY environment variable")
    }
    const deployer_address = wallet.address;
    const weth_addresses = {};

    async function callback(chain, info) {
        await chain.giveToken(deployer_address, "aUSDC", 100e6);
        const contract = await deployContract(
            wallet.connect(chain.provider),
            ConstAddressDeployer
        );
        info.constAddressDeployer = contract.address;
    }

    const toFund = [deployer_address];

    for (let j = 2; j < process.argv.length; j++) {
        toFund.push(process.argv[j]);
    }

    await createAndExport({
        chainOutputPath: "./info/local.json",
        accountsToFund: toFund,
        callback: callback
    });

    fs.copyFile("./info/local.json", "./web/info/local.json", (err) => {
        if (err) throw err;
    });
})();
