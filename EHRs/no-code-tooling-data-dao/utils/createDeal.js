import MarketAPIABI from "../constants/abi/MarketAPI.json";
import chainId from "../constants/chainId";
import { ethers } from "ethers";
import Contracts from "../constants/contracts";
import { getMaxPriorityFeePerGas } from "./getMaxPriorityFeePerGas";

export async function createDeal(provider, signer, id, ipfsHash, endBlock) {
  let maxPriorityFee = await getMaxPriorityFeePerGas(provider);

  const contract = new ethers.Contract(
    Contracts.MarketAPI,
    MarketAPIABI.abi,
    signer
  );

  await contract.create_mock(id, ipfsHash, endBlock, {
    gasLimit: 1000000000,
    maxPriorityFeePerGas: maxPriorityFee?.toString(),
  });
}
