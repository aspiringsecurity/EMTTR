# Optimism NFT Marketplace

Optimism NFT marketplace for enabling sharing of government assets and service/repair of vehicle assets. We are extending and building the nft marketplace using lost and found NFT marketplace example tempalte build on Optimism.

---
## 🏄‍♂️ Getting Started - IDE Setup + Environment Variables

> Clone the repo

> Install
```
cd vehicle-spare-parts-nft-optimism-marketplace
yarn install
```

This 🏗 scaffold-eth fork is pointed at the **Ethereum Mainnet** out of the box (instead of localhost like usual).

Before we do anything, we will set up our environment variables which are needed to interact with both the frontend marketplace and 
contract deployment functionality of this repo. We will be setting up two different .env files, one at the root level of packages/react-app
and one at the root level of packages/hardhat
\
Open up your text editor (this guide assumes VSCode) to better navigate through your files:
```
cd vehicle-spare-parts-nft-optimism-marketplace
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
cd nfctag-uav-optimism-marketplace
yarn start
```



## NFT Metadata Creation + Decentralized File Storage on IPFS

To begin, here is a (lengthy) step-by-step guide to creating and storing your own metadata to IPFS:

- Go to https://www.721.so/
- Click on "create an NFT collection" Artkit flow
- Create New Collection (we will be uploading our own art for this)
- Click the nav burger in the top left corner
- Click "tokens" -> "set collection size" -> set your collection size (set it to 12 if you want it to match with the template NFT smart contract in this repo)
- Uncheck the "File To Duplicate" Box, and click "Update Collection"
- Click the arrow arrow next to the 2 x 2 box that sits underneath the nav burger we clicked previously
- Click on the metadata folder to expand it
- For each token.json file, fill in the name, description, and upload an image file (from your computer). If you don't have any art to use,
use the free-to-use CC0 art from the Lost & Found, Vol. 1 collection! Here's a [link](https://bafybeihcuklo4pmohys7j7xu5uvfpeqgg63wbdkjrx5xrma7mbyy6oxojm.ipfs.dweb.link/) to the ipfs folder holding the .png files (even better, download this art, remix it, and then tag [me](https://twitter.com/0xTranqui) and the [artist](https://twitter.com/dannydiamondss) in your final product when you finish!!!) 
  - For example file sizes, the Lost & Found, Vol. 1 collection used .png files between 3-5 MB, all with 1:1 aspect ratios
  - The "Animation URL" and "External URL" allow you to upload multimedia files / links to static HTML websites, howver you still need to provide\
    an image file to act as a thumbnail if you are planning to link to an animation/external URL
  - You can also add in attributes for each NFT below the main data section
- Obviously repeating this process for large collections could be very time consuming, so luckily there is some nice UI that allows you to shift click    on multiple tokens at once to apply edits to multiple files at the same time\
- Once complete, click on the urger again and go to file -> save as. This will save a .zip copy of your metadata, which you shold store locally for record keeping
- Now that youre metadata is set + saved, go to the burger again and click publish -> upload to IPFS
- You will then get prompted to get an API key from nft.storage (IPFS pinning service). Go and do this -> https://nft.storage/ -> login (make an account) -> API keys -> New key -> copy key and paste back into the API key input we left off on at https://www.721.so/\
- "Click Publish to IPFS!"
- Wait for the files to be pinned, and you'll eventually see two checkmarks that tell you the process is complete. One is for uploading of the assets (.png files), the other is for uploading the metadata (.json files that include a key called "image" who's value-pair is a link to the corresponding .png file for that token).
- You will be prompted with two different things to copy. Save them both for later, we will need them !!! Also follow the links to the assets folder and metadata folder provided at the bottom of the popup and save those links for easy nagivation later as well.\

That's it! At this point, you could actually continue forward creating your own contract with the 721.so contract builder, and then creating your own minting page using the 721.so mint page builder. Both are amazing tools which I have used in the past. For our case, we are going to leave studio https://www.721.so/ to deploy our own smart contract already configured in our packages/hardhat directory of this repo, which we can then mint from using the minting page that is also set up in the website template as well\

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
cd lostandfound-optimism-marketplace
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
cd vehicle-spare-parts-nft-optimism-marketplace
yarn start
```
You'll notice that know images load on the marketplace page. That is because you have to mint some NFTs first before they populate. Head on over to the mint page to mint your pieces (max 2 per wallet with this contract), and then check them out in the marketplace.

DONE !!!!

* Last optional step if you want to deploy a rough website that your friends can mess around with. Run:
```
cd vehicle-spare-parts-nft-optimism-marketplace
yarn build
yarn surge
```

Save the URL and share with your friends. You now have your very own custom NFT project + 0x powered marketplace live on ropsten !!!

Fin 🏁🏁
