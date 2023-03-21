# Optimism NFT Marketplace

Optimism NFT marketplace devtooling for enabling sharing of government R&D assets and service/repair of medical device assets. We are extending and building the nft marketplace using lost and found NFT marketplace [1] example tempalte build on Optimism.

[1] Lost and Found NFT marketplace: https://github.com/0xTranqui/lostandfound-optimism-marketplace

---
##  IDE Setup + Environment Variables

> Clone the repo

> Install
```
cd dicom-optimism-marketplace
yarn install
```

This 🏗 scaffold-eth fork is pointed at the **Ethereum Mainnet** out of the box (instead of localhost like usual).

Before we do anything, we will set up our environment variables which are needed to interact with both the frontend marketplace and 
contract deployment functionality of this repo. We will be setting up two different .env files, one at the root level of packages/react-app
and one at the root level of packages/hardhat
\
Open up your text editor (this guide assumes VSCode) to better navigate through your files:
```
cd dicom-optimism-marketplace
code . 
```
Navigate to + expand the the packages directory, and then right click on the react-app folder and create a new file. Name this file ".env"
Add in the following 2 lines of code:
```
REACT_APP_ALCHEMY_KEY = enter your key here (without quotes)
REACT_APP_ETHERSCAN_KEY= enter your key here (without quotes)
```
This allows the constants.js file in the react-app/src directory to pull these keys into the react-app without needing to expose them publicly.

Navigate to + expand the the packages directory, and then right click on the hardhat folder and create a new file. Name this file ".env"
Add in the following 6 lines of code (the mainnet lines are optional) :
```
ROPSTEN_ALCHEMY_KEY = enter your key here (without quotes)
ROPSTEN_ETHERSCAN_API_KEY = enter your key here (without quotes)
ROPSTEN_DEPLOYER_PRIV_KEY = enter your key here (without quotes)

OPTIMISM_ALCHEMY_KEY = enter your key here (without quotes)
OPTIMISM_ETHERSCAN_API_KEY = enter your key here (without quotes)
OPTIMISM_DEPLOYER_PRIV_KEY = enter your key here (without quotes)
```
This allows the hardhat.config.js file in the packages/hardhat directory to use these keys for the smart contract deployment and 
verification functionality we will be implementing without having to expose them publicly.

## Setting Up the Front End

```
cd dicom-optimism-marketplace
yarn start
```

## SMART CONTRACT DEPLOYMENT + FRONT END INTEGRATION

We are going to skip a lot of steps/background information so that you use what's already in this repo to deploy your own NFT contract as simply as possible. Here is a step by step process for what to do:

- Navigate to packages/hardhat directory
- Double check that your .env at the root level of this directory is set up correctly (look back earlier in this guide if unsure)
- Open hardhat/config.js
- Line 29: Change "mainnet" to "ropsten"
- Line 88-91: Un-comment these 4 lines that relate to the ropsten configuration. If you are unsure how to do this, just mirror the formatting of the optimism configuration that is not commented out in line 101-104. You won't be able to deploy successfully if you do this incorrectly
- Line 101-104: Comment out the 4 lines that relate to the optimism configuration.
- Open hardhat/contracts/lostandfound_vol_1_reloaded.sol
- On line 91 of the contract, replace the contract URI you see here with the contract URI you created during the artkit process! Told you we were going to need this later. Mimic the out-of-the-box formatting of this line exactly
- In your terminal, run:
```
cd packages/hardhat
npx hardhat clean
npx hardhat compile
```
Your contract is almost ready to be deployed. More steps:
- Open hardhat/deploy/deploy_LF_ERC721.js
- On line 12 args, replace the contractURI with the contract URI you saved earlier! (I acknowledge this is repetivive, but I beleive these two inputs are serving slightly different purposes. Or I'm wrong and this is a redundant unncessary step. But if you don't know what you're doing, do this just to be safe)
- We will now deploy the contract. In your terminal, run:
```
cd dicom-optimism-marketplace
yarn deploy
```
- Contract deployed to ropsten! Copy the address your contract was deployed to and save it for the next step.
- We will know verify the contract. In your terminal, run:
```
cd packages/hardhat
npx hardhat verify --network ropsten "insert contract address here w/o quotes" "insert the constructor argument you entered in line 12 of deploy_LF_ERC721.js EXACTLY how you entered it (in quotes)"
```
Your contract is now verified on etherscan! This means other people can see into the innerworkings on your contract now on etherscan (which is great for transparency + having trustworthy code)

Here are the final steps to link your now published NFT contract into your front end (replacing the template project that was included in this repo):
- Navigate back to packages/react-app/src/contracts/external_contracts.js
- Replace the addresss for lostandFoundContract4 on line 6126 with your newly deployed contract. No need to touch the ABI because it is the exact same contract from a structural standpoint
- Navigate to packages/react-app/src/views/App.jsx and replace the contract address on line 71 with your newly deployed contract address
- Save files (I hope you've been saving your files everytime I've been giving you step by step directions lol).
- That's it, you can now run the following code to restart your front end and it will open up the site with your own NFT contract installed:
```
cd dicom-optimism-marketplace
yarn start
```
You'll notice that know images load on the marketplace page. That is because you have to mint some NFTs first before they populate. Head on over to the mint page to mint your pieces (max 2 per wallet with this contract), and then check them out in the marketplace.

DONE !!!!

* Last optional step if you want to deploy a rough website that your friends can mess around with. Run:
```
cd dicom-optimism-marketpalce
yarn build
yarn surge
```

Save the URL and share with your friends. You now have your very own custom NFT project + 0x powered marketplace live on ropsten !!!


