import React from "react";
import { NoWalletDetected } from "./NoWalletDetected";
import "./contracts.css";
import { Link } from 'react-router-dom';

export class Contracts extends React.Component {
  constructor(props) {
    super(props);

    this.initialState = {
      policies: [],
      contract: props.insuranceContract
    };

    this.state = this.initialState;
  }

  formatDate(dateString) {
    return new Intl.DateTimeFormat({
      month: "2-digit",
      day: "2-digit",
      year: "numeric"
    }).format(new Date(dateString));
  }

  formatVote(number) {
    if (number === -1) {
      return <div className="text-danger">Denied</div>;
    }
    if (number === 1) {
      return <div className="text-success">Accepted</div>;
    }
    return <div>No valid vote</div>;
  }

  async componentDidMount() {
    const policies = await this.state.contract.getAllPolicies();
    this.setState({ policies });
    this._getTotalPremiums();
    this._getTotalCapitalInsured();
  }

  async _getTotalCapitalInsured() {
    try {
      const totalCapitalInsured =
        await this.props.insuranceContract.getTotalCapitalInsured();
      this.setState({ totalCapitalInsured });
    } catch (error) {
      console.log(error);
      return;
    }
  }

  async _getTotalPremiums() {
    try {
      const totalPremiums =
        await this.props.insuranceContract.getTotalPremiums();
      this.setState({ totalPremiums });
    } catch (error) {
      console.log(error);
      return;
    }
  }

  decodeStatus(_policyStatusRaw) {
    switch (_policyStatusRaw) {
      case 0: return <div className="badge badge-secondary">CREATED</div>;
      case 1: return <div className="badge badge-info">RUNNING</div>;
      case 2: return <div className="badge badge-primary">COMPLETED</div>;
      case 3: return <div className="badge badge-warning">CLAIMED</div>;
      case 4: return <div className="badge badge-success">PAIDOUT</div>;
      default:return <div className="badge badge-danger">UNKNOWN</div>;
    }
  }

  toPlainString(num) {
    return (''+ +num).replace(/(-?)(\d*)\.?(\d*)e([+-]\d+)/,
      function(a,b,c,d,e) {
        return e < 0
          ? b + '0.' + Array(1-e-c.length).join(0) + c + d
          : b + c + d + Array(e-d.length+1).join(0);
      });
  }

  render() {
    if (window.ethereum === undefined || !this.props.selectedAddress) {
      return <NoWalletDetected />;
    }

    return (
      <div>
        <h1>
          <b>Total Value Insured</b>
        </h1>
        <hr />

        <div className="container">
          <div className="row">
            <div className="col text-light bg-dark mb-2 p-3 rounded-sm shadow border border-white">
              Ocean Storm protocol has earned
              <h2>{parseFloat(this.state.totalPremiums) / 1e18} ETH</h2>
              in paid policy premiums
            </div>
          </div>

          <div className="row">
            <div className="col text-light bg-info mb-2 p-3 rounded-sm shadow border border-white">
              Ocean Storm is now insuring
              <h2>{parseFloat(this.state.totalCapitalInsured) / 1e18} ETH</h2>
              in capital value
            </div>
          </div>

          <div className="row">
            <div className="col form-group p-0">
              <button
                type="button"
                className="btn btn-primary"
                onClick={() => this._getTotalPremiums()}
              >
                Update
              </button>
            </div>
          </div>
        </div>

        <br />

        <h1>
          <b>Status of our Policies</b>
        </h1>
        <hr />

        <div className="tableWrapper">
          <div className="tableWrapper2">
            <table>
              <thead>
                <tr>
                  <th>Cover Id</th>
                  <th>Ship Id</th>
                  <th>Cover Amount</th>
                  <th>Status</th>
                </tr>
              </thead>
              <tbody>
                {this.state.policies.map((policy, index) => {
                  return (
                    <tr key={index}>
                      <td>{policy.policyId.toString()}</td>
                      <td><Link to={`/ship/${policy.ship.id}`}>{policy.ship.id}</Link></td>
                      <td>
                        {this.toPlainString(parseFloat(policy.ship.shipmentValue) / 1e18)} ETH
                      </td>
                      <td>{this.decodeStatus(policy.coverage.status)}</td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
        </div>
      </div>
    );
  }
}
