/*
 * This config file is shared between hardhat and the webapp to ensure that everything stays in parity
 */

import {ethers} from "ethers"

// this default key is a nonsense one from ganache, fear not :)
// it's only used for deploying the contract - you will need to have metamask installed in the browser to use a private key
// for interactions in the web UI
const PRIVATE_KEY = process.env.PRIVATE_KEY || "0x17c77758c7d6ef0f2152e41bc3feba3cfc785147ea69c538b3e213cdde5e2b74"

// the address of the deployed smart contract - you will need to update this after running `npm run deploy`
const contractAddress = "0x9D38f3BB80D98cE09C3f0936Bea140181d4CCABA"

// the amount of FIL that a user will have to pay to enter the raffle
const entryCost = ethers.utils.parseEther("0.005")

// the payout the person who triggers the raffle will receive for doing so
const triggerIncentive = ethers.utils.parseEther("0.001")

// the number of blocks before the raffle in which entries close
const cutoffPeriodInBlocks = 3

// the maximum amount of gas your call can use
const txGasLimit = 44446300

// some configuration for both local and hyperspace test networks
const networks = {
    hyperspace: {
        chainId: 3141,
        url: "https://api.hyperspace.node.glif.io/rpc/v1",
        accounts: [PRIVATE_KEY],
        allowUnlimitedContractSize: true,
    },
    local: {
        chainId: 1337,
        url: "http://127.0.0.1:8545",
        accounts: [PRIVATE_KEY],
        allowUnlimitedContractSize: true,
    }
}

const defaultNetwork = "hyperspace"
export {networks, defaultNetwork, contractAddress, entryCost, triggerIncentive, cutoffPeriodInBlocks, txGasLimit}