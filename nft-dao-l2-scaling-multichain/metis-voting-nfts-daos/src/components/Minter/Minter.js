import { useEffect, useState } from "react";
import { connectWallet } from "./api/connectWallet";
import { mintNFT } from "./api/mint";
import FileUploader from "./uploadImage";

const handleStatus = (walletAddress, fileUrl, name, description, setStatus) => {
  if (window.ethereum) {
    if (walletAddress) {
      if (fileUrl) {
        if (name && description) {
          setStatus("");
        } else {
          setStatus("Please add a name and description");
        }
      } else {
        setStatus("Please add a file and upload it");
      }
    } else {
      setStatus("🦊 Connect to Metamask using the top right button.");
    }
  } else {
    setStatus(
      <p>
        {" "}
        🦊{" "}
        <a
          rel="noreferrer"
          target="_blank"
          href={`https://metamask.io/download.html`}
        >
          You must install Metamask, a virtual Ethereum wallet, in your browser.
        </a>
      </p>
    );
  }
};

const Minter = (props) => {
  //State variables
  const [walletAddress, setWallet] = useState("");
  const [status, setStatus] = useState("");
  const [name, setName] = useState("");
  const [description, setDescription] = useState("");
  const [fileUrl, setFileUrl] = useState("");

  // const getWallet = async () => {
  //   const { address, status } = await getCurrentWalletConnected();
  //   setWallet(address);
  //   if (status) setStatus(status);
  // };

  const connectWalletPressed = async () => {
    const walletResponse = await connectWallet();
    if (status) setStatus(walletResponse.status);
    setWallet(walletResponse.address);
  };

  function addWalletListener() {
    if (window.ethereum) {
      window.ethereum.on("accountsChanged", (accounts) => {
        if (accounts.length > 0) {
          setWallet(accounts[0]);
        } else {
          setWallet("");
        }
      });
    }
  }

  useEffect(() => {
    // getWallet();
    addWalletListener();
  }, []);

  useEffect(() => {
    handleStatus(walletAddress, fileUrl, name, description, setStatus);
  }, [walletAddress, name, fileUrl, description]);

  const onMintPressed = async () => {
    //TODO: implement
    setStatus("Your NFT is minting");
    const { status } = await mintNFT(fileUrl, name, description);
    setStatus(status);
  };

  const mintable = !!(fileUrl && walletAddress && name && description);

  return (
    <div className="wrapper">
      <div className="Minter">
        <button id="walletButton" onClick={connectWalletPressed}>
          {walletAddress.length > 0 ? (
            "Connected: " +
            String(walletAddress).substring(0, 6) +
            "..." +
            String(walletAddress).substring(38)
          ) : (
            <span>Connect Wallet</span>
          )}
        </button>

        <br></br>
        <h1 id="title">🧙‍♂️ Polygon NFT Minter</h1>
        <p style={{ marginTop: "20px" }}>
          Simply choose a file, upload it, add a name, and description, then
          press "Mint."
        </p>
        <FileUploader fileUrl={fileUrl} setFileUrl={setFileUrl} />
        <form>
          <h2>🤔 Name: </h2>
          <input
            type="text"
            placeholder="e.g. My first NFT!"
            onChange={(event) => setName(event.target.value)}
          />
          <h2>✍️ Description: </h2>
          <input
            type="text"
            placeholder="e.g. Even cooler than cryptokitties ;)"
            onChange={(event) => setDescription(event.target.value)}
          />
        </form>
        <button
          disabled={!mintable}
          style={
            !mintable ? { background: "#c7b4e6", borderColor: "#c7b4e6" } : {}
          }
          id="mintButton"
          onClick={onMintPressed}
        >
          Mint NFT
        </button>
        <p id="status">{status}</p>
      </div>
    </div>
  );
};

export default Minter;
