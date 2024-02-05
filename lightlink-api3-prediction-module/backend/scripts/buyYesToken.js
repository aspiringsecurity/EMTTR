const hre = require("hardhat");
const {
  abi: handlerABI,
} = require("../deployments/mumbai/PM_MarketHandler.json");

async function main() {
  const PredictionMarket = await hre.deployments.get("PredictionMarket");
  const PredictionMarketContract = new hre.ethers.Contract(PredictionMarket.address, PredictionMarket.abi, (await hre.ethers.getSigners())[0]);
  // ID of the prediction market you want to buy YES tokens for.
  const id = 1;
  const data = await PredictionMarketContract.getPrediction(id);
  const marketHandlerAddress = data.marketHandler;
  console.log("Market Handler Address: " + marketHandlerAddress);

  const MockUSDC = await hre.deployments.get("MockUSDC");
  const mockUSDCContract = new hre.ethers.Contract(MockUSDC.address, MockUSDC.abi, (await hre.ethers.getSigners())[0]);
  const MarketHandlerContract = new hre.ethers.Contract(marketHandlerAddress, handlerABI, (await hre.ethers.getSigners())[0]);
  
  // Buying 10 YES tokens
  const amount = 10n * 1000000n;
  const toApprove = 50000000n;
    const txn = await mockUSDCContract.approve(marketHandlerAddress, toApprove);
    const receipt = await txn.wait(1);
    console.log("USDC approved for MarketHandler contract: " + receipt.transactionHash);
  try {
    const txn = await MarketHandlerContract.buyYesToken(amount);
    const receipt = await txn.wait(1);
    if (receipt.status == 1) console.log("Bought " + amount/1000000n + " YES tokens");
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
