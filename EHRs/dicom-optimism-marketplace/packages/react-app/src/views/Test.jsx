import React, { useState } from "react";
import { Link } from "react-router-dom";
import { Button, Form, notification, Select } from "antd";

//========== my custom import
import "./Mint.css";
import LF_Logo_Blueprint from "./LF_Logo_Blueprint_0x.png";
import Premint_Artwork from "./Untitled_Artwork.png";
import Confetti from 'react-confetti';
import { useWindowWidth, useWindowHeight } from '@react-hook/window-size';
//========== my custom import

var count = 0; // this saves count that is used to determine what state setting is used when tracking active user mints


//==== new imports for the 0x marketplace
import { NftSwapV4, ETH_ADDRESS_AS_ERC20 } from '@traderxyz/nft-swap-sdk';
import { ethers, BigNumber } from "ethers";


/* import { MsgValueCannotEqualZeroError } from "@0x/utils/lib/src/revert_errors/exchange-forwarder/revert_errors";
const { ERC721Order, NFTOrder } = require("@0x/protocol-utils");
const utils = require("@0x/utils"); */



//== new imports for the 0x marketplace


function OldEnglish({
   readContracts,
   mainnetProvider,
   blockExplorer,
   totalSupply,
   DEBUG,
   writeContracts,
   tx,
   address,
   localProvider,
   oldEnglishContract,
   startBlock,
   userSigner,
   //===my custom imports
   lostandfoundNFTContract,
   priceOfMint,
   maxSupply,
   remainingMints,
   nftContractURI
}) {

   const onFinishFailed = errorInfo => {
      console.log("Failed:", errorInfo);
   };

   //=====MINT FORM + STATE
   const [mintForm] = Form.useForm();
   const [mint, setMint] = useState(false);

   //====SAVING MINT STATE
   const [tokenIdMinted1, setTokenIdMinted1] = useState("");
   const [tokenIdMinted2, setTokenIdMinted2] = useState("");

   //=====Saving image urls for each mint
   const [mintImageURL1, setMintImageURL1] = useState("");
   const [mintImageURL2, setMintImageURL2] = useState("");

   //====saving nft names for each mint
   const [mintName1, setMintName1] = useState("");
   const [mintName2, setMintName2] = useState("");

   //====defining window size for confetti animation
   const width = useWindowWidth();
   const height = useWindowHeight();

   //===fetching metadata
   const fetchMetadataMint1 = async (tokenId) => {
      try {
         const mintedTokenId1 = tokenId;
         const metadataURL1 = "https://ipfs.io/ipfs/" + nftContractURI.substring(7) + "/" + mintedTokenId1 + ".token.json"; 
         const mintMetadataFetch1 = await fetch(metadataURL1);
         
         try {
            const metadataObject1 = await mintMetadataFetch1.json();
            const imageURL1 = "https://ipfs.io/ipfs/" + metadataObject1.image.substring(7);
            const nftName1 = metadataObject1.name;
            setMintImageURL1(imageURL1);
            setMintName1(nftName1);

         } catch(e) {
            console.log(e);
         }
      }  catch (e) {
         console.log(e);
      }
   };

   const fetchMetadataMint2 = async (tokenId) => {
      try {
         const mintedTokenId2 = tokenId;
         const metadataURL2 = "https://ipfs.io/ipfs/" + nftContractURI.substring(7) + "/" + mintedTokenId2 + ".token.json";
         const mintMetadataFetch2 = await fetch(metadataURL2);
         
         try {
            const metadataObject2 = await mintMetadataFetch2.json();
            const imageURL2 = "https://ipfs.io/ipfs/" + metadataObject2.image.substring(7);
            const nftName2 = metadataObject2.name;
            setMintImageURL2(imageURL2);
            setMintName2(nftName2);

         } catch(e) {
            console.log(e);
         }
      }  catch (e) {
         console.log(e);
      }
   };

   //=======0x Protocol Create Order Flow===========

   const createOrder = async () => {

      console.log("createOrder function running");

      const CHAIN_ID = 3; //3 = ropsten

      const lostandfound_token_0 = {
         tokenAddress: '0xd373B9C8acc3439d42359bDAd3a0e3cC4BD0Ff66', //ropsten deployment
         tokenId: '2', //this should be remy
         type: 'ERC721'
      }

      const price_to_list_for = {
         tokenAddress: ETH_ADDRESS_AS_ERC20, //nulladdress so that lister gets paid in eth
         amount: "10000000000000000", //16 zeroes aka 0.01eth
         type: 'ERC20'
      }

      const walletAddressUserA = '0x806164c929Ad3A6f4bd70c2370b3Ef36c64dEaa8'
      const nftToSwapUserA = lostandfound_token_0;

      const walletAddressUserB = '0x153D2A196dc8f1F6b9Aa87241864B3e4d4FEc170'
      const ethToSwapUserB = price_to_list_for;

      const nftSwapSdk = new NftSwapV4(localProvider, userSigner, CHAIN_ID);

   // Check if we need to approve the NFT for swapping
      const approvalStatusForUserA = await nftSwapSdk.loadApprovalStatus(
         nftToSwapUserA,
         walletAddressUserA
      );

      // If we do need to approve User A's NFT for swapping, let's do that now
      if (!approvalStatusForUserA.contractApproved) {
         const approvalTx = await nftSwapSdk.approveTokenOrNftByAsset(
         nftToSwapUserA,
         walletAddressUserA
         );
         const approvalTxReceipt = await approvalTx.wait();
/*          console.log(
         `Approved ${assetsToSwapUserA[0].tokenAddress} contract to swap with 0x v4 (txHash: ${approvalTxReceipt.transactionHash})`
         ); */
      }   

      // Create the order (Remember, User A initiates the trade, so User A creates the order)
      const order = nftSwapSdk.buildOrder(
         nftToSwapUserA,
         ethToSwapUserB,
         walletAddressUserA
      );
      // Sign the order (User A signs since they are initiating the trade)
      console.log("onchainOrder getting made: ")
      const onchainOrder = await nftSwapSdk.exchangeProxy.preSignERC721Order(order); 
      console.log("onchainOrder finished!")
   }

    //=======0x Protocol Cancel Order Flow===========

   const cancelOrder = async () => {
      
      const CHAIN_ID = 3; //3 = ropsten

      const nftSwapSdk = new NftSwapV4(localProvider, userSigner, CHAIN_ID);

      const orderNonce = "100131415900000000000000000000000000000160812848772371044860429851089384113904";
   
      const cancelOrder = await nftSwapSdk.exchangeProxy.cancelERC721Order(orderNonce);
   
   }
   
    //=======0x Protocol Fill Order Flow===========

   const fillOrder = async () => {

      const CHAIN_ID = 3; // 3 = ropsten, 10 = optimism

      console.log ("fillOrder function running");

      const lostandfound_token_0 = {
         tokenAddress: '0xd373B9C8acc3439d42359bDAd3a0e3cC4BD0Ff66', //ropsten nft contract deployment
         tokenId: '0', //this should be angel
         type: 'ERC721'
      }

      const price_to_list_for = {
         tokenAddress: ETH_ADDRESS_AS_ERC20, 
         amount: "10000000000000000", //16 zeroes aka 0.01eth
         type: 'ERC20'
      }      

      const walletAddressUserB = '0x153D2A196dc8f1F6b9Aa87241864B3e4d4FEc170'
      const ethToSwapUserB = price_to_list_for;

      const nftSwapSdk = new NftSwapV4(localProvider, userSigner, CHAIN_ID);

   // Check if we need to approve the NFT for swapping
      const approvalStatusForUserB = await nftSwapSdk.loadApprovalStatus(
         ethToSwapUserB,
         walletAddressUserB
      );      

      if (!approvalStatusForUserB.contractApproved) {
         const approvalTx = await nftSwapSdk.approveTokenOrNftByAsset(
         ethToSwapUserB,
         walletAddressUserB
         );
         const approvalTxReceipt = await approvalTx.wait();
/*          console.log(
         `Approved ${assetsToSwapUserA[0].tokenAddress} contract to swap with 0x v4 (txHash: ${approvalTxReceipt.transactionHash})`
         ); */
      }   

      const reconstructedOnchainOrder = [
         0, // trade direction (0 = sell, 1 = buy)
         "0x806164c929Ad3A6f4bd70c2370b3Ef36c64dEaa8", // maker address
         "0x0000000000000000000000000000000000000000", // taker address 
         "2524604400", // expiry from creation of original order?
         "100131415900000000000000000000000000000334066851805640573173232377402275719802",
          // nonce
         ETH_ADDRESS_AS_ERC20, // erc20token
         "10000000000000000", // erc20 token amount (hardhcoded to 0.01 eth)
         [], // fees (none included atm)
         "0xd373b9c8acc3439d42359bdad3a0e3cc4bd0ff66", // erc721 nft contract
         "1", // erc721 nft contract token id
         [] // erc721 token properties (none included atm)
      ]

      const nullSignatureStruct = {

         // These value indicates that the order maker has previously marked the order as fillable on-chain. The remaining fields in the Signature struct will be ignored.
         // link to where this explanation comes from: https://docs.0x.org/protocol/docs/signatures         
         "r": "0x0000000000000000000000000000000000000000000000000000000000000000",
         "s": "0x0000000000000000000000000000000000000000000000000000000000000000",
         "v": 0,
         "signatureType": 4
      }

      const fillTx = await nftSwapSdk.exchangeProxy.buyERC721(
         reconstructedOnchainOrder,
         nullSignatureStruct,
         "0x", // taker address
         { value: BigNumber.from("10000000000000000").toString() }
      );

      const fillTxReceipt = await nftSwapSdk.awaitTransactionHash(fillTx);
      console.log('Filled order! 🎉', fillTxReceipt.transactionHash); 
   
   }


   return (
      <div className="mint">
         <div className="beforeMintRender">
            <div className="mintPageExplanation">
               <img className="logoWidth" src={LF_Logo_Blueprint}></img>
               <br /> 
               <div className="mintPageExplanationBody">
                  <Button
                  style={{ backgroundColor: "black", color: "white", border: "4px solid black", fontSize: "1.25rem", height: "auto", borderRadius: 20  }} 
                  type="primary"
                  onClick={createOrder}
                  >
                     List Item
                  </Button>
               </div>
               <div className="mintPageExplanationBody">
                  <Button
                  style={{ backgroundColor: "black", color: "white", border: "4px solid black", fontSize: "1.25rem", height: "auto", borderRadius: 20  }} 
                  type="primary"                  
                  >
                     Update Listing
                  </Button>
               </div>
               <div className="mintPageExplanationBody">
                  <Button
                  style={{ backgroundColor: "black", color: "white", border: "4px solid black", fontSize: "1.25rem", height: "auto", borderRadius: 20  }} 
                  type="primary"
                  onClick={cancelOrder}                  
                  >
                     Cancel Listing
                  </Button>
               </div>
               <div className="mintPageExplanationBody">
                  <Button
                  style={{ backgroundColor: "black", color: "white", border: "4px solid black", fontSize: "1.25rem", height: "auto", borderRadius: 20  }} 
                  type="primary"   
                  onClick={fillOrder}               
                  >
                     Buy Item
                  </Button>
               </div>
            </div>                     
         </div>
      </div>
   );
}

export default OldEnglish;