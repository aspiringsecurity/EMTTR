const hre = require("hardhat");

async function main() {
  const Trading = await hre.deployments.get("PredictionMarket");
  const tradingContract = new hre.ethers.Contract(Trading.address, Trading.abi, (await hre.ethers.getSigners())[0]);
  // ID of the prediction market.
  const id = 1;
  const data = await tradingContract.getPrediction(id);
  console.log("Prediction Details:");
  console.log("Marker Handler: " + data.marketHandler);
  console.log("Symbol:         " + data.tokenSymbol);
  console.log("Proxy Address:  " + data.proxyAddress);
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
