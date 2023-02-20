import { ethers } from "ethers";
import SoulboundERC721ABI from "../constants/abi/SoulboundERC721.json";
import DemeritSoulboundERC721ABI from "../constants/abi/DemeritSoulboundERC721.json";
import { getMaxPriorityFeePerGas } from "./getMaxPriorityFeePerGas";

export async function assignSoulboundToken(
  provider,
  signer,
  collectionAddress,
  receiver,
  dealId,
  factoryIndex
) {
  let maxPriorityFee = await getMaxPriorityFeePerGas(provider);

  let artifact;
  let functionName;
  let args;

  switch (factoryIndex) {
    case 0:
    case 1:
      artifact = SoulboundERC721ABI;
      functionName = "issue";
      args = [receiver];
      break;
    case 2:
      artifact = DemeritSoulboundERC721ABI;
      functionName = "assign";
      args = [receiver, dealId];
      break;
  }
  args.push({
    gasLimit: 1000000000,
    maxPriorityFeePerGas: maxPriorityFee?.toString(),
  });

  const contract = new ethers.Contract(collectionAddress, artifact.abi, signer);
  await contract.functions[functionName](...args);
}
