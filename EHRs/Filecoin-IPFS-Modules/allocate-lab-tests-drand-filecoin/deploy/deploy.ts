import "hardhat-deploy"
import "hardhat-deploy-ethers"
import {ethers, network} from "hardhat"
import {cutoffPeriodInBlocks, entryCost, triggerIncentive} from "../src/config"

const sk = network.config.accounts[0]
const wallet = new ethers.Wallet(sk, ethers.provider)

export default async function main() {
    console.log(`wallet address: ${wallet.address}`)
    console.log("deploying dRaffle contract...")

    const DRaffle = await ethers.getContractFactory("DRaffle")
    const raffle = await DRaffle.deploy(cutoffPeriodInBlocks, entryCost, triggerIncentive)

    console.log(`dRaffle contract deployed to: ${raffle.address}`)
}

