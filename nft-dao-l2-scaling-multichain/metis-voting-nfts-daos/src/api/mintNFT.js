import { ethers } from "ethers";
import { useState } from "react";

import {
  contractABIs,
  contract_addresses,
  explorers,
  polygonChainId,
  supportedChainIds,
} from "./constants";

export const ConnectWallet = async () => {
  try {
    const { ethereum } = window;

    if (!ethereum) {
      console.log("Metamask not detected");
      return;
    }
    let chainId = await ethereum.request({ method: "eth_chainId" });
    console.log("Connected to chain:" + chainId);

    if (!supportedChainIds.includes(chainId)) {
      setError("Please use a supported network");
      return;
    }

    const accounts = await ethereum.request({ method: "eth_requestAccounts" });

    return accounts[0];
  } catch (error) {
    console.log("Error connecting to metamask", error);
    return null;
  }
};

// Creates transaction to mint NFT on clicking Mint button
export const useMintNftAction = (dao) => {
  const [isMinting, setisMinting] = useState(false);
  const [minted, setMinted] = useState(false);
  const [txLink, setTxLink] = useState(null);
  const [error, setError] = useState(null);

  const mintNFT = async (chainId) => {
    try {
      const { ethereum } = window;

      if (!ethereum) {
        setError("Having trouble connecting to metamask");
        return;
      }

      const contract_address =
        contract_addresses[ethers.utils.hexlify(chainId)];
      const contractABI = contractABIs[ethers.utils.hexlify(chainId)];
      const explorer = explorers[ethers.utils.hexlify(chainId)];

      const userAddress = await ConnectWallet();
      const provider = new ethers.providers.Web3Provider(ethereum);
      const signer = await provider.getSigner();

      const nftContract = new ethers.Contract(
        contract_address,
        contractABI,
        signer
      );

      const nftTx =
        chainId === parseInt(polygonChainId)
          ? await nftContract.mintChildNFT(
              dao.contract_address || dao.contractAddress,
              userAddress,
              {
                gasLimit: 3_000_000,
                value: ethers.utils.parseEther(String(dao.mintFee)),
              }
            )
          : await nftContract.mintTribNFT(
              dao.contract_address || dao.contractAddress,
              userAddress,
              {
                gasLimit: 3_000_000,
                value: ethers.utils.parseEther(String(dao.mintFee)),
              }
            );

      setisMinting(true);

      let tx = await nftTx.wait();

      setMinted(true);
      setisMinting(false);
      setTxLink(`${explorer}tx/${tx.transactionHash}`);
    } catch (error) {
      console.log("Error minting", error);
      setError(error.message);
    }
  };
  return { isMinting, minted, txLink, error, mintNFT };
};
