const ethers = require("ethers");

const { settlementAddress, settlementABI } = require("../../constants/info");

const PROVIDER = process.env.GOERLI_RPC;
const DEPLOYER = process.env.PK_DEPLOYER;

const provider = new ethers.providers.JsonRpcProvider(PROVIDER);
const wallet = new ethers.Wallet(DEPLOYER, provider);
const settlement = new ethers.Contract(
  settlementAddress,
  settlementABI,
  wallet
);

async function runConclude(id) {
  try {
    const tx = await settlement.concludePrediction_1(id); // Assuming 'conclude' is a state-changing function

    // Wait for the transaction to be mined
    const receipt = await tx.wait(1);
    console.log("Transaction receipt:", receipt);

    return true;
  } catch (error) {
    console.error("Error calling conclude:", error);
    return false;
  }
}

export default async function handler({ req, res }) {
  if (req.method == "POST") {
    const { predictionId } = req.query;

    const callStatus = await runConclude(predictionId);

    if (callStatus) res.status(200).json({ status: callStatus });
    else res.status(400).json({ status: callStatus });
  }
}
