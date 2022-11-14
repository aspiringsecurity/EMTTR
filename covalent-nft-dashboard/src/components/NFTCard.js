import React from 'react';
import { useNavigate } from 'react-router-dom';
import { Col, Card, Tabs, Image, Button } from 'antd';

import { explorerURL } from '../utils/explorer';

function NFTCard({ nftdata, chainIconURL }) {
  const navigate = useNavigate();

  const getURLImage = url => {
    if(url.startsWith("http")){
      return url;
    }
    else{
      return `https://ipfs.io/ipfs/${url}`;
    }
  }

  return (
    <Col xs={12} md={8} lg={6}>
      <Card
        style={{ position: "relative" }}
        hoverable
        cover={<Image className="card-image" alt="NFT Image" src={nftdata.external_data.image ? nftdata.external_data.image : getURLImage(nftdata.token_url)} />}
      >
        <Tabs defaultActiveKey="1">
          <Tabs.TabPane tab="Contract Info" key="1">
            <Card.Meta title={`${nftdata.contract_name} (${nftdata.contract_ticker_symbol})`} description={`Id: ${nftdata.token_id}`} />
            <p>Contract Address: <a href={explorerURL[nftdata.chain_id] + nftdata.contract_address} target="_blank" rel="noopener noreferrer">
              {nftdata.contract_address.substring(0,5) + "..." + nftdata.contract_address.substring(37,42)}
            </a></p>
            <Button type="primary" onClick={() => navigate(`/contract/${nftdata.contract_address}`)}>
              View all NFTs minted
            </Button>
          </Tabs.TabPane>

          <Tabs.TabPane tab="NFT Data" key="2">
            {nftdata.external_data.image
              ? <>
                  <Card.Meta title={nftdata.external_data.name} description={nftdata.external_data.description} />
                </>
              : <p>None</p>
            }
          </Tabs.TabPane>
        </Tabs>
        <img className="chain-icon" src={chainIconURL} />
      </Card>
    </Col>
  )
}

export default NFTCard;
