import React, { useContext } from 'react';

import "./AddressInfo.css";
import NftInfo from './NftInfo';
import TokenTable from './TokenTable';
import InfoContext from '../../context/infoContext';

const AddressInfo = () => {
    const ctx = useContext(InfoContext);
    const link = `https://scope.klaytn.com/account/${ctx.addressInfo.address}`

    let tokenContent = <TokenTable />
    let nftContent = <NftInfo />

    if (ctx.addressInfo.cryptocurrencyData.length !== 0){
        tokenContent = <TokenTable />
    }else{
        tokenContent = <div className='no-token-data'>No Token Info for this address</div>
    }
    if (ctx.addressInfo.nftData.length !== 0){
        nftContent = <NftInfo />
    }else{
        nftContent = <div className='no-token-data'>No NFT Info for this address</div>
    }

    return (
        <div className='address-info'>
            <section>
                <h3><span className="material-icons-sharp">account_balance</span> Address</h3>
                <div className="address">
                    <h4 className='address'>{ctx.addressInfo.address}</h4>
                    <a href={link} target="_blank" rel="noreferrer noopener">
                        <span className="material-icons-sharp">open_in_new</span>
                    </a>
                </div>
            </section>
            <section>
                <h3><span className="material-icons-sharp">token</span> Tokens</h3>
                { tokenContent }
            </section>
            <section>
                <h3><span className="material-icons-sharp">palette</span> NFTs</h3>
                { nftContent }
            </section>
        </div>
    )
}

export default AddressInfo