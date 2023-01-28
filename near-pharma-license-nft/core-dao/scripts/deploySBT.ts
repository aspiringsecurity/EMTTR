import { ethers } from "hardhat";

async function main() {
  const SpendSBT = await ethers.getContractFactory("SpendSBT");
  const spendSBT = await SpendSBT.deploy();

  await spendSBT.deployed();

  console.log(`Deployed SpendSBT to ${spendSBT.address}`);

  const AdminSBT = await ethers.getContractFactory("SpendAdmin");
  const adminSBT = await AdminSBT.deploy(spendSBT.address);

  await adminSBT.deployed();

  console.log(`Deployed AdminSBT to ${adminSBT.address}`);

  // add admin address to spendSbt state
  await spendSBT.setAdminContract(adminSBT.address);

  // mint pre-approved admin NFTs
  const preApprovedAdminAddressList = process.env.PREAPPROVED_ADMIN_LIST?.split(",");
  if (preApprovedAdminAddressList) {
    for (const address of preApprovedAdminAddressList) {
      await adminSBT.safeMint(address, "");
      console.log(`Admin SBT minted for address: ${address}`);
      await new Promise(function (resolve) {
        setTimeout(function () {
          resolve(true);
        }, 5000);
      });
    }
  }
}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
