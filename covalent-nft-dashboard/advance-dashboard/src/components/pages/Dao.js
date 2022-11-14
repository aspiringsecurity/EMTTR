import { useEffect, useState } from "react";
import SideNav from "../SideNav";
import Organizations from "../Organizations";
import { DoughnutChart } from "../charts/Doughnut";
import { PieChart } from "../charts/Pie";

const Dao = () => {
  const [uniswapToken, setUniswapTokens] = useState([]);
  const [network, setNetwork] = useState(1);
  const [dexName, setDexName] = useState("uniswap_v2");
  const [loading, setLoading] = useState(false);

  const setNetworkHandler = (networkId) => {
    // console.log(networkId);
    setNetwork(networkId);
  }

  const setDexNameHandler = (dex) => {
    // console.log(dex);
    setDexName(dex);
  }

  const loadDataHandler = () => {
    getUniswapTokens();
  }

  const getUniswapTokens = async () => {
    setLoading(true);

    const response = await fetch(
      `https://api.covalenthq.com/v1/${network}/xy=k/${dexName}/tokens/?quote-currency=USD&format=JSON&page-number=1&page-size=20&key=${process.env.REACT_APP_COVALENT_KEY}`
    );
    const data = await response.json();
    setUniswapTokens(data.data.items);

    setLoading(false);
  };

  useEffect(() => {
    getUniswapTokens();
    // eslint-disable-next-line
  }, []);

  return (
    <div className="flex">
      <SideNav />

      <div className="flex-1">
        <div className="bg-indigo-200 m-7 p-6">
          <div className="text-4xl text-gray-800 font-medium">
            Track DeFi and DAO Portfolios
          </div>
          <div className="mt-4">
            View all your DeFi investments and DAO analytics in a single
            dashboard!
          </div>
        </div>

        <div className="flex m-7 space-x-6">
          <div className="w-1/2 bg-white p-4">
            <div className="font-bold text-xl text-gray-700">
              Top 5 Tokens On Uniswap Based On Swap Count(24hrs)
            </div>
            <DoughnutChart />
          </div>

          <div className="w-1/2 bg-white p-4">
            <div className="font-bold text-xl text-gray-700">
              Top 9 Decentralized Exchanges On Ethereum Mainnet And Their Swap
              Fees
            </div>
            <PieChart />
          </div>
        </div>

        <Organizations loading={loading} onLoadData={loadDataHandler} onSetNetwork={setNetworkHandler} onSetDexName={setDexNameHandler} tokens={uniswapToken} />
      </div>
    </div>
  );
};

export default Dao;