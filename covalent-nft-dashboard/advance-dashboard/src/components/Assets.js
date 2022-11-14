import { ethers } from "ethers";
import { Fragment } from "react";

const Assets = (props) => {

    // const loading = true;

    if(props.loading) {
        return <p className="text-white text-center text-4xl mb-4">Loading...</p>
    }

    if(props.allAssets.length === 0) {
        return (
            <Fragment>
                <p className="text-white text-center text-4xl mb-4">Search to track your assets accross multiple chains</p>
                <div>
                    <img className="w-full" src="images/new-Converted.gif" alt="" />
                </div>
            </Fragment>
        )
    }

    return (
        <Fragment>
            <div className="flex m-7 space-x-6">
                <div className="bg-white w-full p-4">
                    <div className="font-bold text-lg pb-4">Address crypto assets</div>

                    <table className="table-auto w-full">
                        <thead className="text-left bg-gray-100 text-gray-500 p-2 uppercase font-bold text-xs">
                            <tr>
                                <th>SN</th>
                                <th>Token</th>
                                <th>Ticker</th>
                                <th>Balance</th>
                                <th>Last Transfered</th>
                                <th>Type</th>
                            </tr>
                        </thead>
                        <tbody className="multi-channel-container divide-y divide-gray-200">
                            {props.allAssets.length > 0 && props.allAssets.map((asset, index) => {
                                return (
                                    <tr key={asset.contract_address} className="single-channel py-2">
                                        <td>{index + 1}</td>
                                        <td><img src={asset.logo_url} className="w-8 h-8 rounded-full" alt="" /> {asset.contract_name}</td>
                                        <td>{asset.contract_ticker_symbol}</td>
                                        <td>{ethers.utils.formatUnits(asset.balance, asset.contract_decimals)}</td>
                                        <td>{asset.last_transferred_at}</td>
                                        <td>{asset.type}</td>
                                    </tr>
                                )
                            })}
                        </tbody>
                    </table>
                </div>
            </div>

            <div className="flex m-7 space-x-6">
                <div className="bg-white w-full p-4 overflow-x-scroll">
                    <div className="font-bold text-lg pb-4">Address transactions</div>
                    <table className="table-auto w-full">
                        <thead className="text-left bg-gray-100 text-gray-500 p-2 uppercase font-bold text-xs">
                            <tr>
                                <th>SN</th>
                                <th>From</th>
                                <th>To</th>
                                <th>Hash</th>
                                <th>Gas Paid</th>
                                <th>Value</th>
                            </tr>
                        </thead>
                        <tbody className="multi-channel-container divide-y divide-gray-200">
                            {props.allTransactions.length > 0 && props.allTransactions.map((transaction, index) => {
                                return (
                                    <tr key={transaction.block_height} className="single-channel py-2">
                                        <td>{index + 1}</td>
                                        <td>{transaction.from_address}</td>
                                        <td>{transaction.to_address}</td>
                                        <td>{transaction.tx_hash}</td>
                                        <td>{transaction.gas_spent}</td>
                                        <td>{transaction.value}</td>
                                    </tr>
                                )
                            })}
                        </tbody>
                    </table>
                </div>
            </div>
        </Fragment>
    );
}

export default Assets;