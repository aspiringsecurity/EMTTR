import { ethers } from "hardhat";
import hackatonManagerFactoryContract from "../../dapp/src/constants/contractAddresses.json"

async function main() {
  const chainid = await (await ethers.provider.getNetwork()).chainId

  const hackathonManagerFactoryInstance = await ethers.getContractAt("HackathonManagerFactory", 
  hackatonManagerFactoryContract.hackatonManagerFactoryContract[chainid]);
  
  
  console.log(`HackathonManagerFactory contract connected at: ${hackathonManagerFactoryInstance.address} on chain ${chainid}`);


  const fee = await hackathonManagerFactoryInstance.deploymentFee();
  console.log({fee});


  const tx = await hackathonManagerFactoryInstance.setFee(ethers.utils.parseEther('0.0001'));
  await tx.wait();

const feeAfter = await hackathonManagerFactoryInstance.deploymentFee();
console.log({feeAfter});

  
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
