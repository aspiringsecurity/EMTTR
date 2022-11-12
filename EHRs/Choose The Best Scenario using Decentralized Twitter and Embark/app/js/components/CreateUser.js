import { Button, FormGroup, ControlLabel, FormControl, HelpBlock, Grid, Row, Col, PageHeader } from 'react-bootstrap';
import { withRouter } from 'react-router-dom'
import React, { Component } from 'react';
import FieldGroup from './FieldGroup';

/**
 * Class that renders a form to facilitate the creation 
 * of a user in the contract.
 * 
 * @extends React.Component
 */
class CreateUser extends Component {

  //#region Constructor
  constructor(props, context) {
    super(props, context);

    // initial state
    this.state = {
      isLoading: false,
      username: '',
      description: '',
      usernameHasChanged: false,
      error: ''
    };
  }
  //#endregion

  //#region Component events
  /**
   * Handles the 'Create Account' button click event which
   * sends a transaction to the contract to create a user.
   * 
   * @returns {null}
   */
  _handleClick = async () => {

    this.setState({ isLoading: true });
    const { username, description } = this.state;

    try {
      
      // set up our contract method with the input values from the form
      const createAccount = DTwitter.methods.createAccount(username, description);
      // get a gas estimate before sending the transaction
      const gasEstimate = await createAccount.estimateGas({ from: web3.eth.defaultAccount });
      // send the transaction to create an account with our gas estimate
      // (plus a little bit more in case the contract state has changed).
      const result = await createAccount.send({ from: web3.eth.defaultAccount,  gas: gasEstimate + 1000 });
      // check result status. if status is false or '0x0', show user the tx details to debug error
      // if (result.status && !Boolean(result.status.toString().replace('0x', ''))) { // possible result values: '0x0', '0x1', or false, true
      //   return this.setState({ isLoading: false, error: 'Error executing transaction, transaction details: ' + JSON.stringify(result) });
      // }

      // Completed of async action, set loading state back
      this.setState({ isLoading: false });

      // tell our parent that we've created a user so it
      // will re-fetch the current user details from the contract
      this.props.onAfterUserUpdate();

      // redirect user to the profile update page
      this.props.history.push('/update/@' + username);

    } catch (err) {
      // stop loading state and show the error
      this.setState({ isLoading: false, error: err.message });
    };
  }

  /**
   * When user changes an input value, record that in the state.
   * Additionally, if the username field was updated, perform a
   * check to see if the username already exists in the contract
   * and set the component state accordingly
   * 
   * @param {SyntheticEvent} cross-browser wrapper around the browser’s native event
   * 
   * @return {null}
   */
  _handleChange = async(e) => {
    let state = {};
    const input = e.target.name;
    const value = e.target.value;

    state[input] = value;

    if (input === 'username') {

      state.usernameHasChanged = true;

      if (value.length >= 8) {

        // ensure we're not already loading the last lookup
        if (!this.state.isLoading) {

          // call the userExists method in our contract asynchronously
          DTwitter.methods.userExists(web3.utils.keccak256(value)).call()
          .then((exists) => {
            
            // stop loading state
            state.isLoading = false;

            // show error to user if user doesn't exist
            state.error = exists ? 'Username not available' : '';
            
            this.setState(state);

          }).catch((err) => {
            
            // stop loading state
            state.isLoading = false;

            // show error message to user
            state.error = err.message;

            this.setState(state);
          });

          // set loading state while checking the contract
          state.isLoading = true;
        }

        // we are loading already, do nothing while we wait
        return true;
      }
    }

    this.setState(state);
  }
  //#endregion

  //#region Helper methods
  /**
   * Validates the form. Return null for no state change,
   * 'success' if valid, and error' if invalid.
   * 
   * @return {string} null for no state change, 'success' 
   * if valid, and error' if invalid
   */
  _getValidationState() {

    // considered valid while loading as we don't know yet
    if (this.state.isLoading) return null;

    // check that we have at least 5 characters in the username
    const length = this.state.username.length;
    if (length === 0){
      if(this.state.usernameHasChanged) return 'error';
      return null;
    } 
    if (length <= 5) return 'error';

    // don't allow '@' or spaces
    if(new RegExp(/[@\s]/gi).test(this.state.username)) return 'error';

    // if we have an error, returning 'error' shows the user 
    // the form is in error (red). Conversely, returning 'success'
    // shows the user the form is valid (green).
    return this.state.error.length > 0 ? 'error' : 'success';
  }
  //#endregion

  //#region React lifecycle events
  render() {
    const { isLoading } = this.state;
    let validationState = this._getValidationState();
    let isValid = validationState === 'success' && !isLoading && !this.state.error;
    let feedback = isValid ? 'Username is available' : this.state.error || 'Usernames must be 6 or more characters and cannot include @ or spaces.';

    if (!this.state.usernameHasChanged) feedback = '';

    return (
      <Grid>
        <Row>
          <Col xs={12}>
          <PageHeader>Create a user <small>for { this.props.account }</small></PageHeader>
          </Col>
        </Row>
        <Row>
          <Col xs={12}>
            <form onSubmit={ !isValid ? null : (e) => this._handleClick(e) }>
              <FieldGroup
                type="text"
                value={ this.state.username }
                disabled={ isLoading }
                placeholder="germany2018champs"
                onKeyPress={ (e) => e.key === '@' || e.key === ' ' ? e.preventDefault() : true }
                onChange={ (e) => this._handleChange(e) }
                name="username"
                autoComplete="off"
                label="Desired username"
                validationState={ validationState }
                hasFeedback={ true }
                help={ feedback }
                inputAddOn={
                  {
                    location: 'before',
                    addOn: '@'
                  }
                }
              />
              <FieldGroup
                type="text"
                value={ this.state.description }
                placeholder="Germany for the 2018 World Cup winnnnnn!! 😞"
                onChange={(e) => this._handleChange(e)}
                name="description"
                label="Description"
              />
              <Button
                bsStyle="primary"
                disabled={ !isValid }
                onClick={ !isValid ? null : (e) => this._handleClick(e) }
              >
                { isLoading ? 'Loading...' : 'Create user' }
              </Button>
            </form>
          </Col>
        </Row>
      </Grid>
    );
  }
  //#endregion
}

export default withRouter(CreateUser);