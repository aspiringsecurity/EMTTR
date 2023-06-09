import { ethers } from "hardhat";

const GOERLI_LINK = '0x326C977E6efc84E512bB9C30f76E30c160eD06FB';
const OWNER = '0x968E88df55AcAeC002e3d7c2393F9742e40d94b9';
async function main() {
  const Operator = await ethers.getContractFactory("Operator");
  const operator = await Operator.deploy(GOERLI_LINK,
					 OWNER);

  await operator.deployed();

  console.log(`Operator deployed to ${operator.address}`);
}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
