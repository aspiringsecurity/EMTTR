import Token from "./Token";

const Organizations = (props) => {

  const networkChangeHandler = (e) => {
    props.onSetNetwork(e.target.value);
  }

  const dexChangeHandler = (e) => {
    props.onSetDexName(e.target.value);
  }

  const dexFetchSubmitHandler = (e) => {
    e.preventDefault();

    props.onLoadData();
  }

  if(props.loading) {
    return <p className="text-center text-5xl text-white mb-4">Loading...</p>
  }

  return (
    <div className="flex m-7 space-x-6">
      <div className="bg-white w-full p-4">
        <div className=" flex justify-between font-bold text-lg pb-4">
          <h3>Swap Count and Liquidity of Tokens Traded on Selected Dexes</h3>

          <form onSubmit={dexFetchSubmitHandler}>
            <select onChange={networkChangeHandler}>
                <option value="1">Ethereum</option>
                <option value="56">Binance</option>
                <option value="250">Fantom</option>
                <option value="43114">Avalanche</option>
            </select>

            <select onChange={dexChangeHandler}>
              <option value="uniswap_v2">Uniswap</option>
              <option value="sushiswap">Sushiswap</option>
              <option value="pancakeswap_v2">Pancakeswap</option>
              <option value="spookyswap">Spookyswap</option>
              <option value="traderjoe">Trader Joe</option>
            </select>

            <button type="submit" className="bg-green-500 py-2 px-4 rounded text-white">Load</button>
          </form>
        </div>

        <table className="table-auto w-full">
          <thead className="text-left bg-gray-100 text-gray-500 p-2 uppercase font-bold text-xs">
            <tr>
              <th>SN</th>
              <th>Token</th>
              <th>Quote Rate</th>
              <th>Total Liquidity</th>
              <th>Swap Count</th>
              <th>DEX Name</th>
            </tr>
          </thead>
          <tbody className="multi-channel-container divide-y divide-gray-200">
            {props.tokens.length > 0 &&
              props.tokens.map((token, index) => (
                <Token
                  id={index}
                  key={token.contract_address}
                  decimals={token.contract_decimals}
                  name={token.contract_name}
                  dex_name={token.dex_name}
                  logo={token.logo_url}
                  quote_rate={token.quote_rate}
                  swap_count={token.swap_count_24h}
                  total_liquidity={token.total_liquidity}
                  total_liquidity_quote={token.total_liquidity_quote}
                  total_volume_24h={token.total_volume_24h}
                  total_volume_24h_quote={token.total_volume_24h_quote}
                />
              ))}
          </tbody>
        </table>
      </div>
    </div>
  );
};

export default Organizations;