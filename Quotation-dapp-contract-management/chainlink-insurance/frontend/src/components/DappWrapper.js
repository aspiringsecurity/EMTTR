import React from "react";
import { Routes, Route } from "react-router-dom";
import DelayInsuranceArtifact from "../contracts/InsuranceContract.json";
import contractAddress from "../contracts/contract-address.json";
import { Contract, providers } from "ethers";
import { NoWalletDetected } from "./NoWalletDetected";
import { ConnectWallet } from "./ConnectWallet";
import { Dapp } from "./Dapp";
import { Contracts } from "./Contracts";
import { Ship } from "./Ship";

const HARDHAT_NETWORK_ID = "31337";
const KOVAN_NETWORK_ID = "42";

export class DappWrapper extends React.Component {
  constructor(props) {
    super(props);

    this.initialState = {
      selectedAddress: undefined,
      transactionError: undefined,
      networkError: undefined,
    };

    this.state = this.initialState;
  }

  _initialize(userAddress) {
    // We first store the user's address in the component's state
    this.setState({
      selectedAddress: userAddress,
    });

    this._intializeEthers();
  }

  async _intializeEthers() {
    this._provider = new providers.Web3Provider(window.ethereum);

    this.insuranceContract = await new Contract(
      contractAddress.Contract,
      DelayInsuranceArtifact.abi,
      this._provider.getSigner(0)
    );
  }

  async _connectWallet() {
    // It returns a promise that will resolve to the user's address.
    const [selectedAddress] = await window.ethereum.enable();

    if (!this._checkNetwork()) {
      return;
    }

    this._initialize(selectedAddress);

    // We reinitialize it whenever the user changes their account.
    window.ethereum.on("accountsChanged", ([newAddress]) => {
      // `accountsChanged` event can be triggered with an undefined newAddress.
      // This happens when the user removes the Dapp from the "Connected
      // list of sites allowed access to your addresses" (Metamask > Settings > Connections)
      // To avoid errors, we reset the dapp state
      if (newAddress === undefined) {
        return this._resetState();
      }

      this._initialize(newAddress);
    });

    // We reset the dapp state if the network is changed
    window.ethereum.on("networkChanged", ([networkId]) => {
      this._resetState();
    });
  }

  _dismissNetworkError() {
    this.setState({ networkError: undefined });
  }

  _checkNetwork() {
    if (
      window.ethereum.networkVersion === HARDHAT_NETWORK_ID ||
      window.ethereum.networkVersion === KOVAN_NETWORK_ID
    ) {
      return true;
    }

    this.setState({
      networkError:
        "Please connect Metamask to Localhost:8545 or Kovan network.",
    });

    return false;
  }

  render() {
    if (!this.state.selectedAddress || !this.insuranceContract) {
      return (
        <ConnectWallet
          connectWallet={() => this._connectWallet()}
          networkError={this.state.networkError}
          dismiss={() => this._dismissNetworkError()}
        />
      );
    }

    if (window.ethereum === undefined) {
      return <NoWalletDetected />;
    }

    return (
      <div>
        <div style={{ marginBottom: "1rem" }}>
          Dapp is connected with adress: {this.state.selectedAddress}
        </div>
        <Routes>
          <Route
            path="/"
            element={
              <Dapp
                selectedAddress={this.state.selectedAddress}
                insuranceContract={this.insuranceContract}
              />
            }
          />
          <Route
            path="/contracts"
            element={
              <Contracts
                selectedAddress={this.state.selectedAddress}
                insuranceContract={this.insuranceContract}
              />
            }
          />
          <Route
            path="/ship/:shipId"
            element={
              <Ship />
            }
          />
        </Routes>
      </div>
    );
  }
}
