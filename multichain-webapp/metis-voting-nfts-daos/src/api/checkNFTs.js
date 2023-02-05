import { supportedChainIds } from "./constants";
import { ConnectWallet } from "./mintNFT";

const CheckNFTs = async (contract_address) => {
  const address = await ConnectWallet();
  // use this as default since only tribunal address is defined
  const contractAddress =
    contract_address || import.meta.env.VITE_TEST_CONTRACT_ADDRESS;
  try {
    const res = await fetch(
      // `https://deep-index.moralis.io/api/v2/${address}/nft/${contractAddress}?chain=polygon&format=decimal`,
      `https://deep-index.moralis.io/api/v2/${address}/nft?chain=polygon&format=decimal`,
      {
        method: "GET",
        headers: {
          "X-API-Key": import.meta.env.VITE_MORALIS_WEB3_API_KEY,
        },
      }
    );

    const nfts = (await res.json()).result;

    return nfts;
  } catch (err) {
    console.log({ err });

    return null;
  }
};
export default CheckNFTs;

export const VerifyNFTs = async (contract_address, userAddress) => {
  try {
    // TODO: Impelemnt NFT checker for networks not supported by Moralis
    const chainId = await window.ethereum.request({
      method: "eth_chainId",
    });

    if (supportedChainIds.includes(`${chainId}`)) return true;

    const res = await fetch(
      `https://deep-index.moralis.io/api/v2/${userAddress}/nft/${contract_address}?chain=polygon&format=decimal`,
      {
        method: "GET",
        headers: {
          "X-API-Key": import.meta.env.VITE_MORALIS_WEB3_API_KEY,
        },
      }
    );

    const nfts = (await res.json()).result;

    return !!nfts;
  } catch (err) {
    console.log({ err });

    return null;
  }
};
