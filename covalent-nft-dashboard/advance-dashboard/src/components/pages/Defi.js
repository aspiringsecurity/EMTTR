import { useState } from 'react';

import TopNav from '../TopNav';
import SideNav from '../SideNav';
import Assets from '../Assets';

const Defi = () => {

    const [fetchedTokens, setFetchedTokens] = useState([]);
    const [fetchedTransactions, setFetchedTransactions] = useState([]);
    const [loading, setLoading] = useState(false);

    const setLoadingHandler = (value) => {
        setLoading(value);
    }

    const fetchedTokenHandler = (fetchedTokens) => {
        setFetchedTokens(fetchedTokens);
    }

    const fetchedTransactionHandler = (fetchedTransactions) => {
        setFetchedTransactions(fetchedTransactions);
        console.log(fetchedTransactions);
    }

    return (
        <div className="flex" style={{ minHeight: '100vh' }}>
          <SideNav />

          <div className="flex-1 overflow-x-scroll">
              <TopNav onLoading={setLoadingHandler} onFetchTransaction={fetchedTransactionHandler} onFetchedToken={fetchedTokenHandler} />

              <div className="bg-indigo-200 m-7 p-6">
                    <div className="text-4xl text-gray-800 font-medium">
                        Track DeFi and DAO Portfolios
                    </div>
                    <div className="mt-4">View all your DeFi investments and DAO analytics in a single dashboard!</div>
              </div>

              <Assets loading={loading} allAssets={fetchedTokens} allTransactions={fetchedTransactions} />
          </div>
        </div>
    );
}

export default Defi;