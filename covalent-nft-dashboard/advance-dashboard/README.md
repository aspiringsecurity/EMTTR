
# Web3Dashboard 
Web3Dashboard is a Defi and DAO platform that enables users to keep track of all the details of their DeFi investments, transactions, and assets across all multiple chains and also displays DAO data using a data visualization chart.

# Getting Started with Web3Dashboard
### Web Requirements

## To check your assets you will need the following
Wallet Address.<br>
All Operating systems are supported (Windows, Mac, Linux).<br>
Supported browsers include Chrome, Firefox and Brave.

### User Interface
Once you are in the WebDashbaord, You will see a field to "Enter your wallet address" and select the network to track your assets. Enter your wallet address, select preferred network from the dropdown list and click on the search icon at the top right hand side of the User Interface.
![Dashboard](https://raw.githubusercontent.com/casweeney/Covalent-Hackathon/master/public/images/dashboard1.png)

 

The "Assets" section of the dashboard will load right away after selecting the appropriate button on the dashboard's left side. You can then toggle to the "DAO" section, the NFT session, and back.


#### Key Features
<b>Assets section:</b> Displays the assets you own based the wallet address and selected network. <br>
<b>DAO section:</b>  Displays data from different DAOs used for analysis.<br>
<b>NFT section: (Coming Soon)</b> Displays all the NFTs and address holds.


### Supported Networks
1. Ethereum
2. Binance Smart Chain
3. Polygon
4. Fantom
5. Avalanche


# Development Environment
This project is built with:
1. HTML and CSS
2. Tailwind CSS
3. JavaScript
4. React JS

# Dependencies
1. react-router-dom (used for single page routing)
2. ether.js (used to return token balance by dividing the balance by the decimal)
3. react-chartjs-2 (used for presenting data in charts)

# API
This project was built using the Covalent API

## Endpoints used:
### Class A Endpoints:

1. Get token balances for address
Given the 
chain_id
 and wallet address
, return current token balances for a particular address. This endpoint supports a variety of token standards like ERC20, ERC721 and ERC1155. As a special case, network native tokens like ETH on Ethereum are also returned even though it's not a token contract.

2. Get cryptocurrency transactions for address: Given the 
chain_id
 and wallet 
address
, return all transactions along with their decoded log events. This endpoint queries the blockchain to retrieve all kinds of transactions that references the 
address
 including indexed topics within the event logs.

### Class B Endpoint:

1. Get XY=K network exchange tokens:
Given the chain_id and dexname, we returned network exchange tokens for a specific DEX.

2. Get XY=K supported DEXes.
Returns a list of DEXes currently supported by the XY=K endpoints (chain):
Here we selected Ethereum mainnet as our default chain. We returned top "" DEXes on Ethereum and their swap fees

## User Interface Link
Vercel link []

### Steps to Deploy
1. Clone this repository
2. Run `npm install` to install all dependencies
3. Start a terminal on the project folder
4. Run `npm start` to ensure everything is working well
5. Deploy on (Netlify or Vercel or Fleek)
### Deployments
Project is deployed on Vercel

### Contributors
Casweeney Ojukwu: https://github.com/casweeney <br>
Jesse Raymond: https://github.com/Jesserc <br>
Longs Penum: https://github.com/devlongs <br>
Mitchell Osawaru: https://github.com/Cyber-Mitch
