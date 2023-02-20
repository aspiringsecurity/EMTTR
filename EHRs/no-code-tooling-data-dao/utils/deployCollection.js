import OpenFactoryABI from "../constants/abi/OpenFactory.json";
import DemeritFactoryABI from "../constants/abi/DemeritFactory.json";
import DealFactoryABI from "../constants/abi/DealFactory.json";
import chainId from "../constants/chainId";
import { ethers } from "ethers";
import Contracts from "../constants/contracts";
import { getMaxPriorityFeePerGas } from "./getMaxPriorityFeePerGas";

export async function deployCollection(
  provider,
  signer,
  index,
  name,
  symbol,
  dealId,
  ipfsHash
) {
  let maxPriorityFee = await getMaxPriorityFeePerGas(provider);

  let contractAddress;
  let artifact;
  let functionName;
  let args;

  switch (index) {
    case 0:
      contractAddress = Contracts.OpenFactory;
      artifact = OpenFactoryABI;
      functionName = "createCollection";
      args = [name, symbol, `ipfs://${ipfsHash}`];
      break;
    case 1:
      contractAddress = Contracts.DealFactory;
      artifact = DealFactoryABI;
      functionName = "createSoulboundCollection";
      args = [name, symbol, dealId];
      break;
    case 2:
      contractAddress = Contracts.DemeritFactory;
      artifact = DemeritFactoryABI;
      functionName = "createCollection";
      // TODO: In the future, a dropdown should exist in the "Demerit" case listing all demerits
      let demeritId = ethers.utils.keccak256(
        ethers.utils.toUtf8Bytes("expired-deal")
      );
      args = [name, symbol, `ipfs://${ipfsHash}`, demeritId];
      break;
  }
  args.push({
    gasLimit: 1000000000,
    maxPriorityFeePerGas: maxPriorityFee?.toString(),
  });

  const contract = new ethers.Contract(contractAddress, artifact.abi, signer);
  await contract.functions[functionName](...args);
}
