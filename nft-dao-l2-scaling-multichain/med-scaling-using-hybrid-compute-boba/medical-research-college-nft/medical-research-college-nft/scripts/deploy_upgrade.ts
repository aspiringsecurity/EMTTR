import {artifacts, ethers, upgrades} from "hardhat";

async function main() {

  // Add proxy address here (not implementation)
  // NOTE: NEVER INTERACT WITH THE IMPLEMENTATION CONTRACT DIRECTLY ONLY VIA PROXY
  const previousAddress = "TODO"

  const SBT = await ethers.getContractFactory("UniversityNFT");
  const sbtContract = await upgrades.upgradeProxy(previousAddress, SBT)
  //await sbtContract.deployed();

  console.log("Proxy upgraded to:", sbtContract.address);
  setTimeout(async () => {
    console.log("Implementation deployed to:", await upgrades.erc1967.getImplementationAddress(sbtContract.address))
  }, 1000)
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});