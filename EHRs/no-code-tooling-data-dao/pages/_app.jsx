import "../styles/globals.css";
import { useState, useEffect } from "react";
import { Toaster } from "react-hot-toast";
import { configureChains, createClient, WagmiConfig } from "wagmi";
import { jsonRpcProvider } from "wagmi/providers/jsonRpc";
import dynamic from "next/dynamic";
import SplineObj from "../components/SplineObj";
import NextProgress from "next-progress";

const fvmChain = {
  id: 31415,
  name: "Filecoin — Wallaby testnet",
  network: "wallaby",
  nativeCurrency: {
    decimals: 18,
    name: "Testnet Filecoin",
    symbol: "tFil",
  },
  rpcUrls: {
    default: "https://wallaby.node.glif.io/rpc/v0",
  },
  blockExplorers: {
    default: { name: "Glif", url: "https://explorer.glif.io/wallaby" },
  },
  testnet: true,
};
const { chains, provider, webSocketProvider } = configureChains(
  [fvmChain],
  [
    jsonRpcProvider({
      rpc: (chain) => {
        if (chain.id !== fvmChain.id) return null;
        return { http: chain.rpcUrls.default };
      },
    }),
  ]
);

// Dynamic import of Navbar to avoid SSR issues
const Navbar = dynamic(() => import("../components/Navbar"), {
  ssr: false,
});
let client;
if (typeof window !== "undefined") {
  client = createClient({
    autoConnect: true,
    provider,
    webSocketProvider,
  });
}

const Spline = () => {
  return (
    <div className="h-screen" >
      <SplineObj scene={"/CREWS.splinecode"} />
    </div>
  )
}

function MyApp({ Component, pageProps }) {
  const [isAuthenticated, setIsAuthenticated] = useState(false);
  return (
    <div suppressHydrationWarning className="font-playfair bg-quaternary w-full h-screen overflow-hidden">
      <NextProgress/>
      {typeof window !== "undefined" && client && (
        <WagmiConfig client={client}>
          <div>
            <Navbar suppressHydrationWarning setIsAuthenticated={setIsAuthenticated} />
            <Spline />
            <Component {...pageProps} isAuthenticated={isAuthenticated} Spline={Spline}/>
            <Toaster />
          </div>
        </WagmiConfig>
      )}
    </div>
  );
}

export default dynamic(() => Promise.resolve(MyApp), {
  ssr: false,
});
