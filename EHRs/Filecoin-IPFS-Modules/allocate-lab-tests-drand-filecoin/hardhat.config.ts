import {HardhatUserConfig, task} from "hardhat/config"
import {defaultNetwork, entryCost, networks, txGasLimit} from "./src/config"
import {DRaffle} from "./typechain-types"
import "@nomicfoundation/hardhat-toolbox"
import "hardhat-deploy"
import "hardhat-deploy-ethers"

const config: HardhatUserConfig = {
    solidity: "0.8.18",
    defaultNetwork,
    networks: networks,
};

// hardhat hates you adding imports, because it just pulls in the code for the task manually (ew)
// so although this 'isn't typechecked', it is really
task(
    "draw",
    "draws winners of the drand raffle for the current block",
)
    .addParam("contract", "The address of the draffle contract")
    .setAction(async (taskArgs) => {
        const contractAddr = taskArgs.contract
        const account = network.config.accounts[0]

        const wallet = new ethers.Wallet(account, ethers.provider)

        const dRaffle = await ethers.getContractFactory("DRaffle", wallet)
        const dRaffleContract = await dRaffle.attach(contractAddr)

        const tx = await dRaffleContract.draw({gasLimit: txGasLimit})

        try {
            await tx.wait(1)
            console.log("draw triggered!")
        } catch (err) {
            console.error("it was too early to trigger the draw")
        }
    })

task("enter-draw", "enter the draw for a given address")
    .addParam("contract", "The address of the draffle contract")
    .setAction(async (taskArgs) => {
        const contractAddr = taskArgs.contract
        const account = network.config.accounts[0]

        const wallet = new ethers.Wallet(account, ethers.provider)

        const dRaffle = await ethers.getContractFactory("DRaffle", wallet)
        const dRaffleContract = await dRaffle.attach(contractAddr)
        const tx = await dRaffleContract.enter({value: entryCost})
        await tx.wait(1)
    })

export default config;
