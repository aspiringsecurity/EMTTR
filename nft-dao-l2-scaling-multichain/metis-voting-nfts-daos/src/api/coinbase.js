import { useState } from "react";
import { getProvider } from "../providers/CBProvider";


export const useCoinbaseAuth = () => {
  const [account, setAccount] = useState();
  const [chainId, setChainId] = useState();
  const [provider, setProvider] = useState();
  const [error, setError] = useState();

  /* Connect wallet */
  const CBConnect = async () => {
    try {
      const provider = getProvider();
      setProvider(provider);
      // Get accounts for connected wallet
      const accounts = await provider.request({
        method: "eth_requestAccounts",
      });
      if (accounts) {
        setAccount(accounts[0]);
      }
      // Get current chain ID for connected wallet
      const chainId = await provider.request({
        method: "eth_chainId",
      });
      setChainId(Number(chainId));
    } catch (error) {
      setError(error);
    }
  };


  /* Disconnect wallet */
  const CBDisconnect = () => {
    setAccount("");
    setChainId("");
  };

  return {account, chainId, error, CBConnect, CBDisconnect, provider}
}