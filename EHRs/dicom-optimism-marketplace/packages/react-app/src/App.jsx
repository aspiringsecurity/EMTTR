import {  Menu, Button } from "antd";
import "antd/dist/antd.css";
import {
  useBalance,
  useContractLoader,
  useContractReader,
  useGasPrice,
  useUserProviderAndSigner
} from "eth-hooks";
/* import { useExchangeEthPrice } from "eth-hooks/dapps/dex"; */
import React, { useCallback, useEffect, useState } from "react";
import { Link, Route, Switch, useLocation } from "react-router-dom";
import "./App.css";
import {
  Account,
  Contract,
  ThemeSwitch,
  NetworkDisplay,
  NetworkSwitch,
} from "./components";
import { NETWORKS, ALCHEMY_KEY } from "./constants";
import externalContracts from "./contracts/external_contracts";
import deployedContracts from "./contracts/hardhat_contracts.json";
import { Transactor, Web3ModalSetup } from "./helpers";
import { OldEnglish, Mint, About, Test } from "./views";
import { useStaticJsonRPC } from "./hooks";

//====MY CUSTOM IMPORTS
import linedPaperBackground from "./graph_paper_background_0x.png";
//====MY CUSTOM IMPORTS

const { ethers } = require("ethers");
/*
    Welcome to 🏗 scaffold-eth !

    Code:
    https://github.com/scaffold-eth/scaffold-eth

    Support:
    https://t.me/joinchat/KByvmRe5wkR-8F_zz6AjpA
    or DM @austingriffith on twitter or telegram

    You should get your own Alchemy.com & Infura.io ID and put it in `constants.js`
    (this is your connection to the main Ethereum network for ENS etc.)


    🌏 EXTERNAL CONTRACTS:
    You can also bring in contract artifacts in `constants.js`
    (and then use the `useExternalContractLoader()` hook!)
*/

// 😬 Sorry for all the console logging
const DEBUG = true;
const NETWORKCHECK = true;

const targetNetworkString = "optimism" // <------ change this as you deploy do other networks

const web3Modal = Web3ModalSetup();

// 🛰 providers
const providers = [
  `https://opt-mainnet.g.alchemy.com/v2/${ALCHEMY_KEY}`,
  "https://rpc.scaffoldeth.io:48544",
];

function App(props) {
  const oldEnglishContract = "EightPack";

  //======my custom additions
  const lostandfoundNFTContract = "lostandFoundOptimism"; // update address of lostandFoundContract2 if redeploying an identical nft contract for testing purposes
  const lostandfoundNFTContractAddress = "0xa4248aC1a4Fc557134802f39cddF830Fde6DdA06"; // change this to the nft contract you want to be interacting with
  const zeroExErc721StatusContract = "zeroExErc721StatusOPTIMISM";
  //======my custom additions

  // specify all the chains your app is available on. Eg: ['localhost', 'mainnet', ...otherNetworks ]
  // reference './constants.js' for other networks

  const [injectedProvider, setInjectedProvider] = useState();
  const [address, setAddress] = useState();
  const [selectedNetwork, setSelectedNetwork] = useState(targetNetworkString);
  const location = useLocation();

  /// 📡 What chain are your contracts deployed to?
  const targetNetwork = NETWORKS[selectedNetwork]; // <------- select your target frontend network (localhost, rinkeby, xdai, mainnet)

  // 🔭 block explorer URL
  const blockExplorer = targetNetwork.blockExplorer;

  // load all your providers
  const localProvider = useStaticJsonRPC([
    process.env.REACT_APP_PROVIDER ? process.env.REACT_APP_PROVIDER : targetNetwork.rpcUrl,
  ]);
  const mainnetProvider = useStaticJsonRPC(providers);

  const logoutOfWeb3Modal = async () => {
    await web3Modal.clearCachedProvider();
    if (injectedProvider && injectedProvider.provider && typeof injectedProvider.provider.disconnect == "function") {
      await injectedProvider.provider.disconnect();
    }
    setTimeout(() => {
      window.location.reload();
    }, 1);
  };

  /* 💵 This hook will get the price of ETH from 🦄 Uniswap: */
/*   const price = useExchangeEthPrice(targetNetwork, mainnetProvider); */

  /* 🔥 This hook will get the price of Gas from ⛽️ EtherGasStation */
  const gasPrice = useGasPrice(targetNetwork, "fast");
  // Use your injected provider from 🦊 Metamask or if you don't have it then instantly generate a 🔥 burner wallet.
  const userProviderAndSigner = useUserProviderAndSigner(injectedProvider, localProvider);
  const userSigner = userProviderAndSigner.signer;

  useEffect(() => {
    async function getAddress() {
      if (userSigner) {
        const newAddress = await userSigner.getAddress();
        setAddress(newAddress);
      }
    }
    getAddress();
  }, [userSigner]);

  // You can warn the user if you would like them to be on a specific network
  const localChainId = localProvider && localProvider._network && localProvider._network.chainId;
  const selectedChainId =
    userSigner && userSigner.provider && userSigner.provider._network && userSigner.provider._network.chainId;

  // For more hooks, check out 🔗eth-hooks at: https://www.npmjs.com/package/eth-hooks

  // The transactor wraps transactions and provides notificiations
  const tx = Transactor(userSigner, gasPrice);

  // 🏗 scaffold-eth is full of handy hooks like this one to get your balance:
  const yourLocalBalance = useBalance(localProvider, address);

  // const contractConfig = useContractConfig();

  const contractConfig = { deployedContracts: deployedContracts || {}, externalContracts: externalContracts || {} };

  // Load in your local 📝 contract and read a value from it:
  const readContracts = useContractLoader(localProvider, contractConfig);

  // If you want to make 🔐 write transactions to your contracts, use the userSigner:
  const writeContracts = useContractLoader(userSigner, contractConfig, localChainId);

  //======= CUSTOM FUNCTIONALITY
  const priceOfMint = useContractReader(readContracts, lostandfoundNFTContract, "PRICE");
  const totalSupply = useContractReader(readContracts, lostandfoundNFTContract, "totalSupply");
  const maxSupply =  useContractReader(readContracts, lostandfoundNFTContract, "MAX_SUPPLY");
  
  const remainingMints =  useContractReader(
    readContracts,
    lostandfoundNFTContract,
    "allowedMintCount",
    [address]
  );
  //^this contract read tells the current user how many mints they have left
  
  const nftContractURI =  useContractReader(
    readContracts,
    lostandfoundNFTContract,
    "contractURI"
  );
 //^this contract retreives the contractURI from the NFT contract

  // keep track of a variable from the contract in the local React state:
  const balance = useContractReader(readContracts, oldEnglishContract, "balanceOf", [address]);
  
/*   const addressFromENS = useResolveEnsName(mainnetProvider, "austingriffith.eth");
  console.log("🏷 Resolved austingriffith.eth as:", addressFromENS) */


  const loadWeb3Modal = useCallback(async () => {
    const provider = await web3Modal.connect();
    setInjectedProvider(new ethers.providers.Web3Provider(provider));

    provider.on("chainChanged", chainId => {
      console.log(`chain changed to ${chainId}! updating providers`);
      setInjectedProvider(new ethers.providers.Web3Provider(provider));
    });

    provider.on("accountsChanged", () => {
      console.log(`account changed!`);
      setInjectedProvider(new ethers.providers.Web3Provider(provider));
    });

    // Subscribe to session disconnection
    provider.on("disconnect", (code, reason) => {
      console.log(code, reason);
      logoutOfWeb3Modal();
    });
  }, [setInjectedProvider]);

  useEffect(() => {
    if (web3Modal.cachedProvider) {
      loadWeb3Modal();
    }
  }, [loadWeb3Modal]);

  const faucetAvailable = localProvider && localProvider.connection && targetNetwork.name.indexOf("local") !== -1;

  return (
    <div
    style={{background: `url(${linedPaperBackground})`, backgroundSize: "cover"  }}
    className="App"
    >
      <div className="header">
        <Menu
          style={{ textAlign: "start", backgroundColor: "transparent", borderBottom: "none"/* , border: "2px red solid" */ }} selectedKeys={[location.pathname]} mode="horizontal"
          className="headerMenu"
        >
          <Menu.Item
            style={{ width: "39%", margin: 0, padding: 0 }}
            key="/"
          >
            <Link 
            to="/">
              <button className="marketplaceButton">
                MARKETPLACE
              </button>
            </Link>
          </Menu.Item>

          <Menu.Item 
            style={{ width: "29%", margin: 0, padding: 0 }}         
            key="/mint"
          >
            <Link to="/mint">                
              <button className="mintButton" >
                MINT
              </button>
            </Link>
          </Menu.Item>

          <Menu.Item
            style={{width: "29%", margin: 0, padding: 0 }}
            key="/about"
          >
            <Link to="/about">
              <button className="aboutButton" >
                ABOUT
              </button>
            </Link>
          </Menu.Item>
{/*           <Menu.Item
            style={{width: "29%", margin: 0, padding: 0 }}
            key="/test"
          >
            <Link to="/test">
              <Button>
                Test
              </Button>
            </Link>
          </Menu.Item>  */}         
        </Menu>
        
        <div className="networkAndAccountWrapper">
          <NetworkDisplay
            className="headerNewtorkDisplay"                    
            NETWORKCHECK={NETWORKCHECK}
            localChainId={localChainId}
            selectedChainId={selectedChainId}
            targetNetwork={targetNetwork}
            logoutOfWeb3Modal={logoutOfWeb3Modal}
          />
          <Account
            className="headerAccount"
            style={{height: "auto"}}
            address={address}
            localProvider={localProvider}
            userSigner={userSigner}
            mainnetProvider={mainnetProvider}
            /* price={price} */
            web3Modal={web3Modal}
            loadWeb3Modal={loadWeb3Modal}
            logoutOfWeb3Modal={logoutOfWeb3Modal}
            blockExplorer={blockExplorer}
          />
        </div>       
      </div>      

      <Switch>
        <Route exact path="/">
          <div className="OldEnglishWrapper">
            <OldEnglish
              readContracts={readContracts}
              mainnetProvider={mainnetProvider}
              blockExplorer={blockExplorer}
              totalSupply={totalSupply}
              writeContracts={writeContracts}
              localProvider={localProvider}
              tx={tx}
              address={address}
              DEBUG={DEBUG}
              oldEnglishContract={oldEnglishContract}
              userSigner={userSigner}
              lostandfoundNFTContract={lostandfoundNFTContract}
              lostandfoundNFTContractAddress={lostandfoundNFTContractAddress}
              zeroExErc721StatusContract={zeroExErc721StatusContract}
              priceOfMint={priceOfMint}
              maxSupply={maxSupply}
              balance={balance}
            />
          </div>
        </Route>
        <Route exact path="/mint">
          <div style={{  }}>
            <Mint className="MintWrapper"
              readContracts={readContracts}
              mainnetProvider={mainnetProvider}
              blockExplorer={blockExplorer}
              totalSupply={totalSupply}
              maxSupply={maxSupply}
              remainingMints={remainingMints}
              nftContractURI={nftContractURI}
              writeContracts={writeContracts}
              localProvider={localProvider}
              tx={tx}
              address={address}
              DEBUG={DEBUG}
              oldEnglishContract={oldEnglishContract}
              priceOfMint={priceOfMint}              
              lostandfoundNFTContract={lostandfoundNFTContract}
            />
          </div>
        </Route>     
        <Route exact path="/about">
          <div>
            <About
              readContracts={readContracts}
              mainnetProvider={mainnetProvider}
              blockExplorer={blockExplorer}
              totalSupply={totalSupply}
              maxSupply={maxSupply}
              writeContracts={writeContracts}
              localProvider={localProvider}
              tx={tx}
              address={address}
              DEBUG={DEBUG}
              oldEnglishContract={oldEnglishContract}
            />
          </div>
        </Route>
        <Route exact path="/test">
          <div>
            <Test
              readContracts={readContracts}
              mainnetProvider={mainnetProvider}
              blockExplorer={blockExplorer}
              totalSupply={totalSupply}
              maxSupply={maxSupply}
              writeContracts={writeContracts}
              localProvider={localProvider}
              userSigner={userSigner}
              tx={tx}
              address={address}
              DEBUG={DEBUG}
              oldEnglishContract={oldEnglishContract}
            />
          </div>
        </Route>                 
      </Switch>

{/* COMMENTING OUT HELPFUL SCAFFOLD-ETH TOOLS NOT USED IN THIS PROJECT

<ThemeSwitch /> commenting out theme switch, always dark mode */}
{/*
      // 👨‍💼 Your account is in the top right with a wallet at connect options 
      <div style={{ position: "fixed", textAlign: "right", right: 0, top: 0, padding: 0 }}>
        <div style={{ display: "flex", flex: 1, alignItems: "center" }}>
          <div style={{ marginRight: 20 }}>
            <NetworkSwitch
              networkOptions={networkOptions}
              selectedNetwork={selectedNetwork}
              setSelectedNetwork={setSelectedNetwork}
            />
          </div>
          <Account
            address={address}
            localProvider={localProvider}
            userSigner={userSigner}
            mainnetProvider={mainnetProvider}
            price={price}
            web3Modal={web3Modal}
            loadWeb3Modal={loadWeb3Modal}
            logoutOfWeb3Modal={logoutOfWeb3Modal}
            blockExplorer={blockExplorer}
          />
        </div>
        <FaucetHint localProvider={localProvider} targetNetwork={targetNetwork} address={address} />
      </div>
*/}
      {/* 🗺 Extra UI like gas price, eth price, faucet, and support: */}
{/*       <div style={{ position: "fixed", textAlign: "left", left: 0, bottom: 20, padding: 10 }}>
        <Row align="middle" gutter={[4, 4]}>
          <Col span={8}>
            <Ramp price={price} address={address} networks={NETWORKS} />
          </Col>

          <Col span={8} style={{ textAlign: "center", opacity: 0.8 }}>
            <GasGauge gasPrice={gasPrice} />
          </Col>
          <Col span={8} style={{ textAlign: "center", opacity: 1 }}>
            <Button
              onClick={() => {
                window.open("https://t.me/joinchat/KByvmRe5wkR-8F_zz6AjpA");
              }}
              size="large"
              shape="round"
            >
              <span style={{ marginRight: 8 }} role="img" aria-label="support">
                💬
              </span>
              Support
            </Button>
          </Col>
        </Row>

        <Row align="middle" gutter={[4, 4]}>
          <Col span={24}>
            {
                if the local provider has a signer, let's show the faucet:  *this should be a comment
              faucetAvailable ? (
                <Faucet localProvider={localProvider} price={price} ensProvider={mainnetProvider} />
              ) : (
                ""
              )
            }
          </Col>
        </Row>
      </div> */}



    </div>
  );
}

export default App;
