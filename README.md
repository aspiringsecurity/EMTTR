EMTTR: Secure, Transparent Drug Testing Pipeline (Improving Data Transparency in Drug Testing)

Website: https://sites.google.com/view/emttrservice/

Enabling the bottom of pyramid through empowering pharma companies and the medicaleco-system to do medicine trial testing and clinical trials via blockchain enabled EMTTRs(Electronic Medicine Trial and Test Records as a Service), EHR and Radiology services on the decentralized cloud. EMTTRs as a service aims at providing

■Secure data storage, transparent data movement and data authenticity.

■Improving Data Transparency in Drug Testing Using Ethereum Blockchain

■Enabling healthcare community by empowering pharma companies & the medicaleco-system to do medicine trial testing securely, transparently using blockchain

Blockchain Communication: Axelar for communication between Polygon blockchain and Fantom destination chain, MultiChain. Covalent endpoint for aggregated view (please visit Covalent-NFT-Dashboard which enables us to analyze, observe all NFTs from wallet address in different networks. Links at the bottom of the readme section)


# BlockChain Eco-system

1. Niftykit low code framework tools: Secure data storage, transparent data movement and data authenticity using Niftykit low code tools. Please visit https://github.com/aspiringsecurity/EMTTR/tree/main/EHRs/niftykitxls

2. Optimism NFT marketplace for DICOM images for research organizations and radioligists: Improving Data Transparency in Drug Testing Using Ethereum Blockchain and Optimism NFT marketplace. Please visit https://github.com/aspiringsecurity/EMTTR/tree/main/EHRs/dicom-optimism-marketplace

3. OpenSea Community Tooling: Enabling healthcare community by empowering pharma companies & the medical eco-system to do medicine trial testing securely, transparently using OpenSea Community tooling and APIs. Please visit:

a. OpenSea Community Tooling: https://github.com/aspiringsecurity/EMTTR/tree/main/EHRs/opensea-community-tooling

b. OpenSea APIs: Python script to retrieve nft trasactions event from OpenSea API. Please visit: https://github.com/aspiringsecurity/EMTTR/tree/main/EHRs/opensea-api-nft-sales

4. Chainlink: Workflow tool using ethereum blockchain network, store on IPFS/Filecoin via NFT.Storage, Chainlink for storing offchain metadata, Chainlink Data Feeds and automation for TPA claims and payments. Please visit https://github.com/aspiringsecurity/EMTTR/tree/main/Audit-contracts

We are utilizing Chainlink VRF as follows:

Research Contract Bill Generation: We are utilizing Chainlink Mix to work with Chainlink smart contracts. The bill script will deploy a smart contract to goerli and get a Random number via Chainlink VRF, which can used to identify a unique transaction/order number for the research contract bill.

Parametric Insurance Solution for medicine discovery with special procedures. We are utilizing an existing example at chainlink github repo to develop an insurance solution for contract researchers. Link: https://github.com/aspiringsecurity/EMTTR/tree/main/Audit-contracts/chainlink-insurance

5. IPFS; Ethereum and Embark; Status 


Features:

Just in Time Service: Availability of pharma companies medicine records across differentstakeholder through secure blockchain network.

• Record Management: Quality documentation reduces the issues regarding testing proceduresand standardization.• Research: Enabling the healthcare community by empowering pharma companies & themedical eco-system to do medicine trial testing securely, transparently.

• Data Security: Efficiently sharing of data (including personal data), privacy concerns andpatient enrollment strategies.

• Transparency: Improving Data Transparency in Drug Testing Using Blockchain.

A greater and more seamless flow of information within a digital drug discovery infrastructure,created by electronic medicine trial and test records as a service (EMTTRs), encompasses andleverages digital progress and can transform the way medicines are developed, tested anddistributed to improve the global health economy and achievement of Sustainable DevelopmentGoals in Healthcare.

Moralis deployment URL: https://3gtivnurtulj.usemoralis.com:2053/server (to be deployed)

Improving Data Transparency in Drug Testing Using Blockchain Smart Contracts

Package contents
================

src/Regulator.sol - the solidity smart contract code for the Regulator contract

src/ClinicalTrial.sol - the solidity code smart contract code for the a Clinical Trial contract

src/start-ethereum-node.sh - script to start a local Ethereum node

src/workflow.js - the main nodejs script that interacts with the blockchain

src/run-workflow.sh - script that will execute the workflow steps

src/read-from-blockchain.sh - script that reads trial data from the blockchain

logs/run-workflow.log - output from running src/run-workflow.sh

logs/read-from-blockchain.log - output from running src/read-from-blockchain.sh

Getting started
===============

This code has been tested on OSX 10.11.5 and Ubuntu 14.04. Code is written using Bash and Javascript - you will need to install nodejs:

https://nodejs.org

We use the testrpc npm module in order to reproduce the exact conditions the workflow scripts need to run the tests. It is important that you reset the blockchain every time you run through these steps. In order to reset, just restart the start-ethereum-node.sh script.

Running the scripts
===================

The steps needed are as follows:

1) Run 'npm install' to install all dependencies mentioned in package.json

2) Install 'testrpc' as a global npm module

npm install -g ethereumjs-testrpc

3) Run the script to start a local Ethereum node

./start-ethereum-node.sh

4) In another terminal, run the script that will execute the workflow steps

./run-workflow.sh

This script will:

	-Deploy a Regulator smart contract
	
	-Add a CRO/pharma to this contract - in this case Roche
	
	-Upload the trial protocol (data/TrialProtocol.pdf) to IPFS, running locally
	
	-Deploy a Clinical Trial smart contract - in this case for Tamiflu, including the IPFS hash linking to the protocol
	
	-Add 500 subjects to the trial
	
	-Add 5 data points for each subject

5) Run the script to read data from the blockchain contracts

./read-from-blockchain.sh

...............................
blockNumber= 3005

drug name= Tamiflu

ipfs hash= QmTZKpCqqFcUsmXWzffmLkYNCwrYkTvhkAXdquzNWij1z4

number of subjects= 501

patient ident= s0/1985-04-03

patient ident= s1/2006-4-1
	 
	 data=  44/mg/89/NONE added at 2016-10-04T16:14:49+01:00
	 
	 data=  50/mg/68/NAUSEA added at 2016-10-04T16:14:49+01:00
	 
	 data=  27/mg/44/HEARTBURN added at 2016-10-04T16:14:49+01:00
	 
	 data=  42/mg/33/COMA added at 2016-10-04T16:14:49+01:00
	 
	 data=  96/mg/54/HEADACHE added at 2016-10-04T16:14:49+01:00

patient ident= s2/2007-5-13

	 data=  10/mg/29/COMA added at 2016-10-04T16:14:49+01:00
	 
	 data=  79/mg/44/NAUSEA added at 2016-10-04T16:14:49+01:00
	 
	 data=  53/mg/21/HEADACHE added at 2016-10-04T16:14:49+01:00
	 
...............................

6) Log files for these scripts can be found in logs directory


# Blockchain Eco-system II

1. Covalent End Point: Covalent-NFT-Dashboard enables us to analyze, observe all NFTs from wallet address of Contract Research Organizations (CROs), pharmaceutical companies in different networks. Please visit https://github.com/aspiringsecurity/EMTTR/tree/main/covalent-nft-dashboard

EthMed Covalent Dashboard: We are developing a Contract Research DAO platform using Covalent that enables healthcare providers, contract researchers and aggregators and pharma organizations to keep track of all the details of their medicine assets, trial budgets, DeFi investments, transactions, and assets across all multiple chains and also displays DAO data using a data visualization chart. We are building our solution on top of an existing web3 dashboard system and are planning to integrate it with an analytics, tabulation and collaboration tool namely EtherCalc. Please visit: https://github.com/aspiringsecurity/EMTTR/tree/main/covalent-nft-dashboard/advance-dashboard

2. Axelar, Polygon, Fantom: Axelar for communication between Polygon blockchain (pharmaceutical companies and Contract research organizations) and Fantom destination chain (compliance system by government organizations like Ministry of Healthcare or Ministry of Commerce). Please visit https://github.com/aspiringsecurity/EMTTR/tree/main/axelar-messaging . Polygon audit smart contracts at https://github.com/aspiringsecurity/EMTTR/tree/main/Audit-contracts/Audit-Polygon

3. Binance: Binance audit smart contracts for government organizations to regulate the tenders and proposals compliance between pharmaceutical companies and CROs (contract research organizations) at https://github.com/aspiringsecurity/EMTTR/tree/main/Audit-contracts/Audit-BSC

4. MultiChain: MultiChain Webapp integration with EtherCalc for analysis, tabulation, graphing, charting and visualization. Please visit the link at https://github.com/aspiringsecurity/EMTTR/tree/main/multichain-webapp/multichain-webapp

5. Tron DAO: DAOTooling using Tron DAO. Please visit https://github.com/aspiringsecurity/EMTTR/tree/main/EHRs/tron-contracts

6. Covalent Dashboard: We are developing a Contract Research DAO platform using Covalent that enables healthcare providers, contract researchers and aggregators and pharma organizations to keep track of all the details of their medicine assets, trial budgets, DeFi investments, transactions, and assets across all multiple chains and also displays DAO data using a data visualization chart. We are building our solution on top of an existing web3 dashboard system and are planning to integrate it with an analytics, tabulation and collaboration tool namely EtherCalc. Please visit: https://github.com/aspiringsecurity/EMTTR/tree/main/covalent-nft-dashboard/advance-dashboard

7. OpenZepellin Governance + Evmos Smart Contracts: Please visit https://github.com/aspiringsecurity/EMTTR/tree/main/EHRs/evmos-smart-contract
   Governor Bravo Model Implementation and Integration: Please visit: https://github.com/aspiringsecurity/EMTTR/tree/main/EHRs/governance






