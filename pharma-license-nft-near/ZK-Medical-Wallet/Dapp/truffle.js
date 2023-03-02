var Web3 = require("web3");
var provider = new Web3.providers.HttpProvider("https://excalibursb2pz2block.blockchain.azure.com:3200/Zlihv_PEgvSkmq51HcTfTq3z");
var web3 = new Web3(provider);

// web3.personal.unlockAccount('')
const HDWalletProvider = require('@truffle/hdwallet-provider');

module.exports = {


    networks: {
        development: {
            host: "localhost",
            port: 8545,
            network_id: "*" // Match any network id
        },

        azure: {
            provider: new HDWalletProvider(
                "emotion wall cash clown nut tongue project picnic public arch blush inform",
                "https://excalibursb2pz2block.blockchain.azure.com:3200/Zlihv_PEgvSkmq51HcTfTq3z"
            ),
            // consortium_id: 
            network_id: "*",
            gas:0,
            gasPrice: 0,
        },

        // azure: {
        //     provider, network_id: "*",
        // }
    }
};
