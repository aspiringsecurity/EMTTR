import { ethers } from "ethers";
import { useState } from "react";
import {
  contractABIs,
  contractDPConstant,
  contract_addresses,
  explorers,
  polygonChainId,
  supportedChainIds,
} from "./constants";

// Creates transaction to create a Tribunal
export const useCreateTribunalAction = () => {
  const [isCreating, setisCreating] = useState(false);
  const [created, setcreated] = useState(false);
  const [txLink, setTxLink] = useState(null);
  const [error, setError] = useState(null);
  const [newTrib, setNewTrib] = useState(null);

  const createTrib = async (tribunalName, walletAddress, fileUrl, mintFee) => {
    let newTribAddress = undefined;
    try {
      const { ethereum } = window;

      if (!ethereum) {
        setError("Having trouble connecting to metamask");
        return;
      }

      const chainId = await ethereum.request({
        method: "eth_chainId",
      });

      const contract_address = contract_addresses[chainId];
      const contractABI = contractABIs[chainId];
      const explorer = explorers[chainId];

      if (!supportedChainIds.includes(chainId)) {
        setError("Please use a supported network");
        return;
      }

      const provider = new ethers.providers.Web3Provider(ethereum);
      const signer = await provider.getSigner();

      const tribContract = new ethers.Contract(
        contract_address,
        contractABI, // use list due to https://github.com/ethers-io/ethers.js/issues/1238
        signer
      );

      const tribTx =
        chainId === polygonChainId
          ? await tribContract.createTrib(walletAddress, fileUrl, mintFee, {
              gasLimit: 3_000_000,
            })
          : await tribContract.createTribunal(
              tribunalName,
              fileUrl,
              mintFee * contractDPConstant,
              walletAddress,
              {
                gasLimit: 3_000_000,
              }
            );

      setisCreating(true);

      let tx = await tribTx.wait();
      setcreated(true);
      console.log("Mined!", tx);
      console.log("Mined!", tribTx);

      const iface = new ethers.utils.Interface(contractABI);

      tx.logs.map((log, i) => {
        try {
          let decodedData = iface.parseLog(log);
          if (decodedData.name === "childContract" && decodedData.args[0]) {
            setNewTrib(decodedData.args[0]);
            newTribAddress = decodedData.args[0];
          }
        } catch (e) {
          console.warn({ e });
        }
      });

      setTxLink(`${explorer}tx/${tribTx.hash}`);
      setisCreating(false);

      return [newTribAddress, chainId];
    } catch (error) {
      console.log("Error creating", { error });
      setError(error.message);
      return;
    }
  };
  return { isCreating, created, txLink, error, createTrib, newTrib };
};
