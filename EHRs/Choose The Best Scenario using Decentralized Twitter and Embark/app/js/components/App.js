import Header from './Header'
import Main from './Main'
import React, { Component } from 'react';
import { withRouter } from 'react-router-dom'
import imgAvatar from '../../img/avatar-default.png';
import { map } from 'async';

/**
 * Class representing the highest order component. Any user
 * updates in child components should trigger an event in this
 * class so that the current user details can be re-fetched from
 * the contract and propagated to all children that rely on it
 * 
 * @extends React.Component
 */
class App extends Component {

  //#region Constructor
  constructor(props) {
    super(props);

    this.state = {
      user: {},
      account: '',
      error: {},
      userAccounts: [],
      balance: 0
    }
  }
  //#endregion

  //#region Helper methods
  /**
   * Loads user details from the contract for all accounts on the node.
   * 
   * For each account on the node, first, the owners mapping is queried using the 
   * owner address key. It returns the hash of the username it maps to. This 
   * username hash is then used to query the users mapping in the contract to 
   * get the details of the user. Once the user details are returned, the state 
   * is updated with the details, which triggers a render in this component and 
   * all child components.
   * 
   * @returns {null}
   */
  _loadCurrentUserAccounts = async () => {

      // get all the accounts the node controls
      const accounts = await web3.eth.getAccounts();

      // Generates a mapping of users and accounts to be used
      // for populating the accounts dropdown
      await map(accounts, async function (address, next) {
        try {
          // gets the balance of the address
          let balance = await web3.eth.getBalance(address);
          balance = web3.utils.fromWei(balance, 'ether');

          // get the owner details for this address from the contract
          const usernameHash = await DTwitter.methods.owners(address).call();

          // get user details from contract
          const user = await DTwitter.methods.users(usernameHash).call();

          // update user picture with ipfs url
          user.picture = user.picture.length > 0 ? EmbarkJS.Storage.getUrl(user.picture) : imgAvatar;

          // add the following mapping to our result
          next(null, {
            address: address,
            user: user,
            balance: balance
          });
        }
        catch (err) {
          next(err);
        }
      }, (err, userAccounts) => {
        if (err) return this._onError(err, 'App._loadCurrentUserAccounts');

        const defaultUserAccount = userAccounts.find((userAccount) => {
          return userAccount.address === web3.eth.defaultAccount;
        });

        this.setState({
          userAccounts: userAccounts,
          user: defaultUserAccount.user,
          account: web3.eth.defaultAccount,
          balance: defaultUserAccount.balance
        });
      });
  }

  /**
   * Sets the App state error and redirects the user to the error page
   * 
   * @param {Error} err - error encountered
   */
  _onError(err, source) {
    if (source) err.source = source;
    this.setState({ error: err });
    this.props.history.push('/whoopsie');
  }
  //#endregion

  //#region React lifecycle events
  componentDidMount() {
    EmbarkJS.onReady(() => {
      setTimeout(() => { this._loadCurrentUserAccounts(); }, 0);
    });
  }

  render() {
    return (
      <div>
        <Header
          user={this.state.user}
          account={this.state.account}
          userAccounts={this.state.userAccounts}
          balance={this.state.balance}
          error={this.state.error}
          onAfterUserUpdate={(e) => this._loadCurrentUserAccounts()}
          onError={(err, source) => this._onError(err, source)} />
        <Main
          user={this.state.user}
          account={this.state.account}
          userAccounts={this.state.userAccounts}
          error={this.state.error}
          onAfterUserUpdate={(e) => this._loadCurrentUserAccounts()}
          onError={(err, source) => this._onError(err, source)} />
      </div>
    );
  }
  //#endregion
}

export default withRouter(App)