import AddressOracleABI from "../constants/abi/AddressOracle.json";
import chainId from "../constants/chainId";
import { ethers } from "ethers";
import Contracts from "../constants/contracts";
import { getMaxPriorityFeePerGas } from "./getMaxPriorityFeePerGas";

export async function addToAddressOracle(provider, signer, address) {
  let maxPriorityFee = await getMaxPriorityFeePerGas(provider);

  const contract = new ethers.Contract(
    Contracts.AddressOracle,
    AddressOracleABI.abi,
    signer
  );

  await contract.setF0Address(address, "t01109", {
    gasLimit: 1000000000,
    maxPriorityFeePerGas: maxPriorityFee?.toString(),
  });
}
