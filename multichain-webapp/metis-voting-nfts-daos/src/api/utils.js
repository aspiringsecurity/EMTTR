import { Web3Storage } from "web3.storage";
import { polygonChainId } from "./constants";

export const getProvider = () => {
  const coinbaseWallet = new CoinbaseWalletSDK({
    appName: "GameHaus",
    appLogoUrl: "https://i.imgur.com/KGYPsq3.png",
  });
  return coinbaseWallet.makeWeb3Provider();
};

const getVerificationMessage = async (address, statement, chainId) => {
  const url = "https://authapi.moralis.io/challenge/request/evm";
  const options = {
    method: "POST",
    headers: {
      accept: "application/json",
      "content-type": "application/json",
      "X-API-KEY": import.meta.env.VITE_MORALIS_WEB3_API_KEY,
    },
    body: JSON.stringify({
      timeout: 15,
      domain: "tribunals.vercel.app",
      chainId,
      address,
      uri: window.location.origin,
      statement,
    }),
  };
  const data = await (await fetch(url, options)).json();

  return data;
};

export const mmAuthenticate = async () => {
  const { ethereum } = window;
  const accounts = await ethereum.request({
    method: "eth_requestAccounts",
  });
  const chainId = await ethereum.request({
    method: "eth_chainId",
  });

  if (accounts[0] && chainId) {
    const msg = "Sign in to Tribunals";

    /**
     * Moralis doesn't support  or  yet
     * TODO: Build custom auth backend for other supported chains
     */

    const { id, message, profileId } = await getVerificationMessage(
      accounts[0],
      msg,
      polygonChainId
    );

    const signature = await ethereum.request({
      method: "personal_sign",
      params: [message, accounts[0]],
    });

    const url = "https://authapi.moralis.io/challenge/verify/evm";
    const options = {
      method: "POST",
      headers: {
        accept: "application/json",
        "content-type": "application/json",
        "X-API-KEY": import.meta.env.VITE_MORALIS_WEB3_API_KEY,
      },
      body: JSON.stringify({ message, signature }),
    };

    const user = await (await fetch(url, options)).json();
    return user;
  }
};

export const cbAuthenticate = async () => {
  const provider = getProvider();
  const accounts = await provider.request({
    method: "eth_requestAccounts",
  });
  const chainId = await provider.request({
    method: "eth_chainId",
  });

  if (accounts[0] && chainId) {
    const msg = "Sign in to Tribunals";
    const { id, message, profileId } = await getVerificationMessage(
      accounts[0],
      msg,
      chainId
    );

    const signature = await provider.request({
      method: "personal_sign",
      params: [message, accounts[0]],
    });

    const url = "https://authapi.moralis.io/challenge/verify/evm";
    const options = {
      method: "POST",
      headers: {
        accept: "application/json",
        "content-type": "application/json",
        "X-API-KEY": import.meta.env.VITE_MORALIS_WEB3_API_KEY,
      },
      body: JSON.stringify({ message, signature }),
    };

    const user = await (await fetch(url, options)).json();

    return user;
  }
};

export const truncateWithEllipsis = (s, n, type) => {
  const maxLen = n !== undefined ? n : 12;

  if (s?.length > maxLen) {
    if (type === "end") {
      return `${s.substr(0, maxLen)}...`;
    }
    return `${s.substr(0, Math.floor(maxLen / 2))}...${s.substr(
      s.length - Math.floor(maxLen / 2)
    )}`;
  }
  return s;
};

export const scrollToTop = () => {
  window.scrollTo({ top: 0, left: 0, behavior: "smooth" });
};

export const getImgUrl = (url) =>
  url.includes("ipfs.infura.io")
    ? `https://w3s.link/ipfs/${url.split("/ipfs/")[1]}`
    : url;

export const uploadtoIPFS = async (data) => {
  const client = new Web3Storage({
    token: import.meta.env.VITE_WEB3STORAGE_TOKEN,
  });

  const blob = new Blob([JSON.stringify(data)], { type: "application/json" });
  const files = [new File([blob], "data.json")];

  const cid = await client.put(files);
  return cid;
};
