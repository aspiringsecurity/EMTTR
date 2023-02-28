async function main() {

  const DelayInsurance = await ethers.getContractFactory("DelayInsurance");
  const delayInsurance = await DelayInsurance.deploy("0xa36085F69e2889c224210F603D836748e7dC0088");
  const [deployer] = await ethers.getSigners();
  console.log("Deploying contracts with the account: " + deployer.address);
  await delayInsurance.deployed();
  console.log("DelayInsurance deployed to:", delayInsurance.address);

  saveFrontendFiles(delayInsurance);
}

function saveFrontendFiles(delayInsuranceContract) {
  const fs = require("fs");
  const contractsDir = __dirname + "/../frontend/src/contracts";

  if (!fs.existsSync(contractsDir)) {
    fs.mkdirSync(contractsDir);
  }

  fs.writeFileSync(
    contractsDir + "/contract-address.json",
    JSON.stringify({ Contract: delayInsuranceContract.address }, undefined, 2)
  );

  const DelayInsuranceArtifact = artifacts.readArtifactSync("DelayInsurance");

  fs.writeFileSync(
    contractsDir + "/InsuranceContract.json",
    JSON.stringify(DelayInsuranceArtifact, null, 2)
  );
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
