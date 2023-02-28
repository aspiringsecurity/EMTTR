import React from "react";

import { providers, utils } from "ethers";

import { NoWalletDetected } from "./NoWalletDetected";
import { TransactionErrorMessage } from "./TransactionErrorMessage";

import ports from "../api/ports.json";
import vessels from "../api/vessels.json";

const ERROR_CODE_TX_REJECTED_BY_USER = 4001;

export class Dapp extends React.Component {
  constructor(props) {
    super(props);

    const initialStateFilled = {
      transactionError: undefined,
      networkError: undefined,
      shipId: "4ae72971-77b3-9167-973e-1da1c773ad41",
      shipName: "QUEEN MARY",
      shipmentValue: "1000",
      departurePort: "MALAGA",
      departureDate: "2021-12-01",
      arrivalPort: "CHENNAI",
      arrivalDate: "2021-12-31",
      policyId: "",
      policyStatus: ""
    };

    const initialStateEmpty = {
      transactionError: undefined,
      networkError: undefined,
      shipId: "",
      shipName: "",
      shipmentValue: "",
      departurePort: "",
      departureDate: "",
      arrivalPort: "",
      arrivalDate: "",
      policyId: "",
      policyStatus: ""
    };

    this.state = initialStateEmpty;
  }

  render() {
    if (window.ethereum === undefined || !this.props.selectedAddress) {
      return <NoWalletDetected />;
    }

    return (
      <>
        <div className="row">
          <div className="col-12">
            <h2>
              🚢 <b>Ocean Storm</b>
            </h2>
          </div>
        </div>

        <hr />

        <div className="row">
          <div className="col-12">
            <p>Subscribe to a Ocean Storm policy today!</p>
          </div>
        </div>

        <hr />

        <div className="form-group">
          <div className="form-row">
            <div className="col-sm-6">
              <label htmlFor="inputShipName">Ship Name</label>
              <div style={{ display: "flex" }}>
                <input
                  type="text"
                  className="form-control"
                  id="inputShipName"
                  aria-describedby="shipNameHelp"
                  placeholder="Ship Name"
                  value={this.state.shipName}
                  onChange={(e) =>
                    !this.state.shipObject
                      ? this.setState({ shipName: e.target.value })
                      : null
                  }
                />
                <button
                  className={`btn ${
                    this.state.shipObject ? "btn-primary" : "btn-secondary"
                  }`}
                  disabled={!this.state.shipObject}
                  onClick={() =>
                    this.setState({
                      shipName: "",
                      shipObject: undefined
                    })
                  }
                >
                  x
                </button>
              </div>

              <small id="shipNameHelp" className="form-text text-muted">
                Search for a vessel and select it
              </small>
              <div>
                {vessels.data.vessels
                  .filter((vessel) => {
                    return (
                      vessel.name.includes(this.state.shipName.toUpperCase()) &&
                      this.state.shipName !== "" &&
                      !this.state.shipObject
                    );
                  })
                  .map((vessel, index) => {
                    return (
                      <div
                        key={index}
                        className="dropdown-item"
                        onClick={() =>
                          this.setState({
                            shipName: vessel.name,
                            shipId: vessel.uuid,
                            shipObject: vessel
                          })
                        }
                      >
                        {vessel.name}
                      </div>
                    );
                  })}
              </div>
            </div>
          </div>
          <div className="form-row">
            <div className="col-sm-6">
              <label htmlFor="inputShipId">Ship Id</label>
              <input
                type="text"
                className="form-control"
                id="inputShipId"
                aria-describedby="shipIdHelp"
                placeholder="Ship Id"
                value={this.state.shipId}
                onChange={(e) => this.setState({ shipId: e.target.value })}
              />
              <small id="shipIdHelp" className="form-text text-muted">
                Type in a Ship Id or search for a vessel in the first input
              </small>
            </div>
            <div className="col-sm-6">
              <label htmlFor="inputShipmentValue">Amount Insured</label>
              <input
                type="text"
                className="form-control"
                id="inputShipmentValue"
                aria-describedby="shipmentValueHelp"
                placeholder="Shipment Value"
                value={this.state.shipmentValue}
                onChange={(e) =>
                  this.setState({ shipmentValue: e.target.value })
                }
              />
              <small id="shipmentValueHelp" className="form-text text-muted">
                The shipment value you want to insure
              </small>
            </div>
          </div>

          <div className="form-row">
            <div className="col-sm-6">
              <label htmlFor="inputDeparturePort">Port of departure</label>
              <div style={{ display: "flex" }}>
                <input
                  type="text"
                  className="form-control"
                  id="inputDeparturePort"
                  aria-describedby="departurePortHelp"
                  placeholder="Port of departure"
                  value={this.state.departurePort}
                  onChange={(e) =>
                    !this.state.departurePortObject
                      ? this.setState({ departurePort: e.target.value })
                      : null
                  }
                />
                <button
                  className={`btn ${
                    this.state.departurePortObject
                      ? "btn-primary"
                      : "btn-secondary"
                  }`}
                  disabled={!this.state.departurePortObject}
                  onClick={() =>
                    this.setState({
                      departurePort: "",
                      departurePortObject: undefined
                    })
                  }
                >
                  x
                </button>
              </div>
              <small id="departurePortHelp" className="form-text text-muted">
                Type the port name and select one port
              </small>
              <div>
                {ports.data
                  .filter((port) => {
                    return (
                      port.port_name.includes(
                        this.state.departurePort.toUpperCase()
                      ) &&
                      this.state.departurePort !== "" &&
                      !this.state.departurePortObject
                    );
                  })
                  .map((port, index) => {
                    return (
                      <div
                        key={index}
                        className="dropdown-item"
                        onClick={() =>
                          this.setState({
                            departurePort: port.port_name,
                            departurePortObject: port
                          })
                        }
                      >
                        {port.port_name}
                      </div>
                    );
                  })}
              </div>
            </div>

            <div className="col-sm-6">
              <label htmlFor="inputDepartureDate">
                Expected departure date
              </label>
              <input
                type="date"
                max="2100-12-31"
                min="2021-01-01"
                className="form-control"
                id="inputDepartureDate"
                aria-describedby="departureHelp"
                placeholder="Expected departure date"
                value={this.state.departureDate}
                onChange={(e) =>
                  this.setState({ departureDate: e.target.value })
                }
              />
              <small id="departureHelp" className="form-text text-muted">
                Select the departure date
              </small>
            </div>
          </div>

          <div className="form-row">
            <div className="col-sm-6">
              <label htmlFor="inputArrivalPort">Port of arrival</label>
              <div style={{ display: "flex" }}>
                <input
                  type="text"
                  className="form-control"
                  id="inputArrivalPort"
                  aria-describedby="arrivalPortHelp"
                  placeholder="Port of arrival"
                  value={this.state.arrivalPort}
                  onChange={(e) =>
                    !this.state.arrivalPortObject
                      ? this.setState({ arrivalPort: e.target.value })
                      : null
                  }
                />
                <button
                  className={`btn ${
                    this.state.arrivalPortObject
                      ? "btn-primary"
                      : "btn-secondary"
                  }`}
                  disabled={!this.state.arrivalPortObject}
                  onClick={() =>
                    this.setState({
                      arrivalPort: "",
                      arrivalPortObject: undefined
                    })
                  }
                >
                  x
                </button>
              </div>
              <small id="departurePortHelp" className="form-text text-muted">
                Type the port name and select one port
              </small>
              <div>
                {ports.data
                  .filter((port) => {
                    return (
                      port.port_name.includes(
                        this.state.arrivalPort.toUpperCase()
                      ) &&
                      this.state.arrivalPort !== "" &&
                      !this.state.arrivalPortObject
                    );
                  })
                  .map((port, index) => {
                    return (
                      <div
                        key={index}
                        className="dropdown-item"
                        onClick={() =>
                          this.setState({
                            arrivalPort: port.port_name,
                            arrivalPortObject: port
                          })
                        }
                      >
                        {port.port_name}
                      </div>
                    );
                  })}
              </div>
            </div>

            <div className="col-sm-6">
              <label htmlFor="inputExpectedDeparture">
                Expected arrival date
              </label>
              <input
                type="date"
                max="2100-12-31"
                min="2021-01-01"
                className="form-control"
                id="inputExpectedDeparture"
                aria-describedby="arrivalHelp"
                placeholder="Expected arrival date"
                value={this.state.arrivalDate}
                onChange={(e) => this.setState({ arrivalDate: e.target.value })}
              />
              <small id="arrivalHelp" className="form-text text-muted">
                Select the arrival date
              </small>
            </div>
          </div>
        </div>

        <div className="row">
          <div className="col-12">
            {this.state.transactionError && (
              <TransactionErrorMessage
                message={this._getRpcErrorMessage(this.state.transactionError)}
                dismiss={() => this._dismissTransactionError()}
              />
            )}
          </div>
        </div>

        <hr />

        <div className="form-group">
          <button
            type="button"
            className="btn btn-primary"
            onClick={(event) => this._subscribePolicy(event)}
          >
            Buy Policy
          </button>
        </div>
      </>
    );
  }

  _dismissTransactionError() {
    this.setState({ transactionError: undefined });
  }

  _getRpcErrorMessage(error) {
    if (error.data) {
      return error.data.message;
    }

    return error.message;
  }

  _resetState() {
    this.setState(this.initialState);
  }

  async _subscribePolicy(event) {
    event.preventDefault();

    // TODO Validate the fields here or somewhere else
    if (this.state.shipId === "") {
      console.log("Invalid field(s)");
      return;
    }

    // TODO
    // customer shoudn't be able to change this value
    // hardvalue for a catnat event (occure 1/200) same as SmartContract
    // we may change this soon
    const INSURANCE_NUMBER_DEFAULT = 200;

    let insuredSum = Math.round(
      this.state.shipmentValue / INSURANCE_NUMBER_DEFAULT
    );
    let overrides = { value: insuredSum };

    try {
      const provider = new providers.Web3Provider(window.ethereum);
      const signer = provider.getSigner();
      const chainId = await signer.getChainId();
      if (chainId === "31337") overrides.chainId = "31337";

      await this.props.insuranceContract.subscribePolicy(
        this.state.shipId,
        this.state.shipmentValue,
        new Date(this.state.departureDate).getTime() / 1000,
        new Date(this.state.arrivalDate).getTime() / 1000,
        utils.formatBytes32String(this.state.departurePort),
        utils.formatBytes32String(this.state.arrivalPort),
        overrides
      );
      window.alert("Transaction success!");
    } catch (error) {
      if (error.code === ERROR_CODE_TX_REJECTED_BY_USER) {
        console.log("User rejected the transaction.");
        return;
      }
      console.log(error);
    }
  }

  async _updatePolicyStatus() {
    try {
      // This will return a Policy (in array format) if it exists on the blockchain side
      const getPolicy = await this.props.insuranceContract.getPolicy();

      // array index based on 'Policy' struct (smart contract code)
      let _policyId = parseInt(getPolicy[0], 16); //hex number
      let _policyStatusRaw = getPolicy[2][7];
      let _policyStatus = "";

      // array index based on 'PolicyStatus' enum (smart contract code)
      switch (_policyStatusRaw) {
        case 0:
          _policyStatus = "CREATED";
          break;
        case 1:
          _policyStatus = "RUNNING";
          break;
        case 2:
          _policyStatus = "COMPLETED";
          break;
        case 3:
          _policyStatus = "CLAIMED";
          break;
        case 4:
          _policyStatus = "PAIDOUT";
          break;
        default:
          _policyStatus = "UNKNOWN";
      }

      if (false /* _policyId == 0 */) {
        window.alert(
          "You don't have policy registered or it is still being created."
        );
      } else {
        this.setState({ policyId: _policyId, policyStatus: _policyStatus });
      }
    } catch (error) {
      console.log(error);
      this.setState({ transactionError: error });
      return;
    }
  }

  async _updateParameters() {
    try {
      this.props.insuranceContract.setInsuranceParameters(
        this.state.incidentsThreshold,
        this.state.keepersInterval,
        this.props.selectedAddress,
        this.state.policyThreshold
      );
    } catch (error) {
      console.log(error);
      this.setState({ transactionError: error });
      return;
    }
  }

  async _updateContracts() {
    try {
      await this.props.insuranceContract.UpdateContracts();
    } catch (error) {
      console.log(error);
      return;
    }
  }

  async _pricePremium() {
    let _policyPrice;
    try {
      _policyPrice = await this.props.insuranceContract.pricePremium(
        {
          id: this.state.shipId,
          shipmentValue: this.state.shipmentValue
        },
        0,
        0,
        0,
        0
      );
      if (_policyPrice)
        this.setState({ policyPrice: `${_policyPrice.toString()} Wei` });
    } catch (error) {
      console.log(error);
      return;
    }
  }
}
