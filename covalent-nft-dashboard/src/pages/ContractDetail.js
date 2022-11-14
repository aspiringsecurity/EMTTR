import React, { useEffect, useState } from 'react';
import { useParams } from 'react-router-dom';
import { Table } from 'antd';

import { NFTPORT_APIKEY, COVALENT_APIKEY } from '../config';

function ContractDetail({ type }) {
  const { contractaddress } = useParams();
  const [contractData, setContractData] = useState({});
  const [nfts, setNFTs] = useState([]);

  useEffect(() => {
    if(contractaddress){
      getContractInfo();
      getContractNFTs();
    }
  }, [contractaddress])

  const columns = [
    {
      title: 'Token ID',
      dataIndex: 'token_id',
      render: text => <p className="table-p">{text}</p>,
    },
    {
      title: 'File URL',
      dataIndex: 'file_url',
      render: url => <a href={url} target="_blank" rel="noopener noreferrer"><p className="table-url">{url}</p></a>,
    },
    {
      title: 'Metadata URL',
      dataIndex: 'metadata_url',
      render: url => <a href={url} target="_blank" rel="noopener noreferrer"><p className="table-url">{url}</p></a>,
    },
  ];

  const getContractInfo = async () => {
    const nft = await fetch(`https://api.covalenthq.com/v1/${type}/nft_market/collection/${contractaddress}/?&key=${COVALENT_APIKEY}`);
      const { data } = await nft.json();
      console.log(data);
      setContractData(data.items[0]);
  }

  const getContractNFTs = async () => {
    const options = {
      method: 'GET',
      headers: {
        "Content-Type": "application/json",
        "Authorization": NFTPORT_APIKEY
      },
    };

    const res = await fetch(`https://api.nftport.xyz/v0/nfts/${contractaddress}?` + new URLSearchParams({
        chain: 'polygon',
        page_size: 50,
        include: "all"
      }), options)
    let nftData = await res.json();
    nftData.nfts.forEach((nft, index) => {
      nft.key = index + 1;
    });
    console.log(nftData);
    setNFTs(nftData.nfts);
  }

  return (
    <div>
      <h1>{contractData.collection_name} ({contractData.collection_ticker_symbol}) {contractData.collection_address}</h1>
      <p>Floor Price: ${contractData.floor_price_quote_7d}</p>
      <p>Unique Token Ids Sold Today: {contractData.unique_token_ids_sold_count_day}</p>
      <p>Gas Quote Rate Day: {contractData.gas_quote_rate_day}</p>
      <Table columns={columns} dataSource={nfts} size="middle" expandable />
    </div>
  )
}

export default ContractDetail;
