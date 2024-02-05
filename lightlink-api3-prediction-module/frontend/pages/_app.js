import { ChakraProvider } from "@chakra-ui/react";
import "@fontsource/barlow/600.css";

import { useState, useEffect } from "react";

const PROJECT_ID = process.env.WALLET_CLOUD_PROJECT_ID;

import { ToastContainer } from "react-toastify";
import "react-toastify/dist/ReactToastify.css";

import "@rainbow-me/rainbowkit/styles.css";
import { getDefaultWallets, RainbowKitProvider } from "@rainbow-me/rainbowkit";
import { configureChains, createConfig, WagmiConfig } from "wagmi";
import { goerli } from "wagmi/chains";
import { publicProvider } from "wagmi/providers/public";
// import { jsonRpcProvider } from "wagmi/providers/jsonRpc";

const { chains, publicClient } = configureChains(
  [goerli],
  [
    // jsonRpcProvider({
    //   rpc: (polygonMumbai) => ({
    //     http: "https://rpc-mumbai.maticvigil.com",
    //   }),
    // }),
    publicProvider(),
  ]
);

const { connectors } = getDefaultWallets({
  appName: "Prediction Market",
  projectId: PROJECT_ID,
  chains,
});

const wagmiConfig = createConfig({
  autoConnect: true,
  connectors,
  publicClient,
});

export default function App({ Component, pageProps }) {
  const toastStyle = {
    fontFamily: "Barlow",
  };

  const [ready, setReady] = useState(false);
  useEffect(() => {
    setReady(true);
  }, []);

  return (
    <>
      {ready ? (
        <>
          <ChakraProvider>
            <WagmiConfig config={wagmiConfig}>
              <RainbowKitProvider chains={chains} modalSize="compact">
                <Component {...pageProps} />
                <ToastContainer
                  toastStyle={toastStyle}
                  position="top-center"
                  autoClose={8000}
                  hideProgressBar={false}
                  newestOnTop={false}
                  closeOnClick={false}
                  rtl={false}
                  pauseOnFocusLoss={false}
                  draggable={false}
                  pauseOnHover={false}
                  theme="light"
                />
              </RainbowKitProvider>
            </WagmiConfig>
          </ChakraProvider>
        </>
      ) : null}
    </>
  );
}
