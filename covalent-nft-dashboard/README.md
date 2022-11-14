# Nft Showcases
A dapp where you can see all NFTs from wallet address in different networks.

## Features
- Users can views their NFTs in different network when they enter their address and select the network such as Ethereum, Polygon (Matic), Avalanche, and Binance Smart Chain
- Users can view the contract of the NFT such as floor price, unique token ids sold today, gas quote rate day, and list of NFTs minted

## Technologies
- React
- Ant Design
- Covalent API
- NFTPort API

## Running the web app on local host
- Clone or download this repository
- Run `npm i` to install the dependencies
- Create a file called 'config.js' on the src folder and add the following code
```
export const NFTPORT_APIKEY= "< Your NFT PORT API key >";
export const COVALENT_APIKEY= "< Your Covalent API key >";
```
- Run `npm start` to start the web app