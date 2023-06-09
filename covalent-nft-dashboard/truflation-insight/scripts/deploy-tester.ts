import { ethers } from "hardhat";

async function main() {
  const contractName = "TruflationTester";
  const Contract = await ethers.getContractFactory(contractName);
  const instance = await Contract.deploy(
    '0x7911771BE70C8CD2298433A27737E2b62E806961',
    'cef7d7ad405e45eb91e2da0f415c920e',
    '10000000000000000'
  );

  await instance.deployed();
  console.log(`${contractName} deployed to ${instance.address}`);
}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
