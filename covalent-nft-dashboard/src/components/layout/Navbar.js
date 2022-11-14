import React from 'react';
import { Link } from 'react-router-dom';
import { Layout, Input, Select } from 'antd';

import { COVALENT_APIKEY } from '../../config';
import { explorerIcon } from '../../utils/explorer';

function Navbar({ type, setUserNFTs, setNFTLoading, setType, setChainIconURL }) {
  const loadMyCollection = async address => {
    try{
      setNFTLoading(true);
      const nft = await fetch(`https://api.covalenthq.com/v1/${type}/address/${address}/balances_v2/?nft=true&key=${COVALENT_APIKEY}`);
      const { data } = await nft.json();
      console.log(data);

      let nftData = [];
      data.items.forEach(item => {
        if(item.nft_data){
          item.nft_data.forEach(nft => {
            nft.contract_address = item.contract_address;
            nft.contract_name = item.contract_name;
            nft.contract_ticker_symbol = item.contract_ticker_symbol;
            nft.chain_id = data.chain_id;
          })
          nftData = nftData.concat(item.nft_data);
        }
      });

      console.log(nftData);
      setUserNFTs(nftData || []);
      setNFTLoading(false);
    } catch(error) {
      console.error(error);
      setNFTLoading(false);
    }
  }

  const changeType = value => {
    console.log(`selected ${value}`);
    setType(value);
  }

  const onSearch = value => {
    console.log(value);
    setChainIconURL(explorerIcon[type]);
    loadMyCollection(value);
  };

  return (
    <Layout.Header className="primary-bg-color navbar">
      <Link to="/" className="logo secondary-color">
        NFT Showcases
      </Link>
      <div style={{ display: 'flex'}}>
        <Select
          placeholder="Select a Network"
          optionFilterProp="children"
          onChange={changeType}
          filterOption={(input, option) =>
            option.children.toLowerCase().indexOf(input.toLowerCase()) >= 0
          }
        >
          <Select.Option value="1">Ethereum Mainnet</Select.Option>
          <Select.Option value="42">Ethereum Testnet Kovan</Select.Option>
          <Select.Option value="137">Matic Mainnet</Select.Option>
          <Select.Option value="80001">Matic Testnet Mumbai</Select.Option>
          <Select.Option value="43114">Avalanche C-Chain Mainnet</Select.Option>
          <Select.Option value="43113">Avalanche Fuji Testnet</Select.Option>
          <Select.Option value="56">Binance Smart Chain</Select.Option>
          <Select.Option value="97">Binance Smart Chain Testnet</Select.Option>
        </Select>
        <Input.Search placeholder="Find NFTs by address" onSearch={onSearch} style={{ width: '350px' }} enterButton/>
      </div>
    </Layout.Header>
  )
}

export default Navbar;
