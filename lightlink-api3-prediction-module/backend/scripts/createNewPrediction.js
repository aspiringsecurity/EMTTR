const hre = require("hardhat");

async function main() {
  const Trading = await hre.deployments.get("PredictionMarket");
  const tradingContract = new hre.ethers.Contract(Trading.address, Trading.abi, (await hre.ethers.getSigners())[0]);

  const MockUSDC = await hre.deployments.get("MockUSDC");
  const mockUSDCContract = new hre.ethers.Contract(MockUSDC.address, MockUSDC.abi, (await hre.ethers.getSigners())[0]);

  const toApprove = 50000000n;
  try {
    const txn = await mockUSDCContract.approve(tradingContract.address, toApprove);
    const receipt = await txn.wait(1);
    console.log("USDC approved for trading contract: " + receipt.transactionHash);
  } catch (err) {
    console.error(err);
  }

  // Setting the prediction market parameters. Head over to https://market.api3.org to get the proxy address for the asset you want to create a prediction market for.
  const symbol = ethers.utils.formatBytes32String("BTC");
  const proxyAddress = "0xe5Cf15fED24942E656dBF75165aF1851C89F21B5";
  const isAbove = ethers.utils.defaultAbiCoder.encode(["bool"], [false]);
  const targetPrice = ethers.utils.parseUnits("30000", "ether").toString();
  // Make sure the deadline timestamp is in the future.
  const deadline = "1705229409";
  const basePrice = "110";

  try {
    const txn = await tradingContract.createPrediction(
      symbol,
      proxyAddress,
      isAbove,
      targetPrice,
      deadline,
      basePrice,
      // {
      //   gasLimit: 10000000,
      // }
    );
    const receipt = await txn.wait(1);
    console.log("Prediction Market Created: " + receipt.transactionHash);
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