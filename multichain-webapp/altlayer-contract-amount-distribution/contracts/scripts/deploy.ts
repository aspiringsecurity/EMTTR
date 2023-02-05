import { ethers } from "hardhat";
import hackatonManagerFactoryContract from "../../dapp/src/constants/contractAddresses.json"

async function main() {
  const chainid = await (await ethers.provider.getNetwork()).chainId

  const HackathonManagerFactoryInstance = await ethers.getContractFactory("HackathonManagerFactory");
  const HackathonManagerFactoryContract = await HackathonManagerFactoryInstance.deploy();

  await HackathonManagerFactoryContract.deployed();

  console.log(`HackathonManagerFactory contract deployed at: ${HackathonManagerFactoryContract.address} on chain ${chainid}`);


  hackatonManagerFactoryContract.hackatonManagerFactoryContract[chainid] = HackathonManagerFactoryContract.address;
  
  const fs = require('fs').promises

  fs.writeFile("../dapp/src/constants/contractAddresses.json",
    JSON.stringify(hackatonManagerFactoryContract, null, 4) )

  // let tx = await HackathonManagerFactoryContract.createNewHack("new hack");
  // tx.wait();

  // let newHackAddress = await HackathonManagerFactoryContract.getHackContractAddress("new hack");
  // console.log(`[deploy test] A new hackathon by the name: new hack - at address ${newHackAddress}`);
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
