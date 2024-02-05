const hre = require("hardhat");

async function main() {
  const Settlement = await hre.deployments.get("PM_Settlement");
  const settlementContract = new hre.ethers.Contract(
    Settlement.address,
    Settlement.abi,
    (await hre.ethers.getSigners())[0]
  );

  try {
    // Prediction Market ID. If the deadline has passed, the prediction market will be concluded. If not, it will be reverted.
    const id = 1;
    const txn = await settlementContract.concludePrediction_1(id,
    {
      gasLimit: "100000",
    }
    );
    const receipt = await txn.wait(1);
    console.log("Prediction Market Concluded: " + receipt.transactionHash);
  } catch (err) {
    console.error(err);
  }
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });