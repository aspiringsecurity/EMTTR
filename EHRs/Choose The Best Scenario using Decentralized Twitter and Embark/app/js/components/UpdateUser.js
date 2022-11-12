import { Button, FormGroup, ControlLabel, FormControl, HelpBlock, Image, Grid, Col, Row, PageHeader } from 'react-bootstrap';
import { withRouter } from 'react-router-dom'
import React, { Component } from 'react';
import FieldGroup from './FieldGroup';

class UpdateUser extends Component {

  //#region Constructor
  constructor(props, context) {
    super(props, context);

    // initial state
    this.state = {
      isLoading: false,
      picture: '',
      description: this.props.user.description,
      error: '',
      formState: null,
      formUpdated: false
    };
  }
  //#endregion

  //#region Component events
  /**
   * Handles the 'Update user' button click event which
   * sends a transaction to the contract to update the 
   * user's profile.
   * 
   * @returns {null}
   */
  _handleClick = async () => {
    // if the form has not been updated, do nothing
    if (!this.state.formUpdated) return;
    
    // show loading state
    this.setState({ isLoading: true });

    // if the user has updated their photo, try to upload it to ipfs
    // and use the resulting ipfs hash to send to the contract as part
    // of the user's profile.
    let hash = '';
    if (this.state.picture !== '') {
      try {
        // upload the file to ipfs and get the resulting hash
        hash = await EmbarkJS.Storage.uploadFile([this.inputPicture]);;
      }
      catch (err) {
        // stop loading state and show user the error
        return this.setState({ isLoading: false, formState: 'error', error: err.message });
      }
    }

    const { account, user } = this.props;
    const { description } = this.state;
    // get a handle for the editAccount method
    const editAccount = DTwitter.methods.editAccount(web3.utils.keccak256(user.username), description, hash);
    // get a gas estimate for the transaction with the input username
    // and description
    const gasEstimate = await editAccount.estimateGas({ from: web3.eth.defaultAccount });

    try {
      // send the transaction with our gas estimate (plus a little bit more in case the contract)
      // state has changed since we got our estimate
      const result = await editAccount.send({ from: web3.eth.defaultAccount, gas: gasEstimate + 1000 });
      // if (result.status && !Boolean(result.status.toString().replace('0x', ''))) {
      //   return this.setState({ isLoading: false, formState: 'error', formUpdated: false, error: 'Error executing transaction, transaction details: ' + JSON.stringify(result) });
      // }

      // stop loading state, and render the form as successful
      this.setState({ isLoading: false, formState: 'success', formUpdated: false });

      // tell parent we've updated our user, so the current
      // user is re-fetched to get the user's details
      return this.props.onAfterUserUpdate();
    }
    catch (err) {
      // stop loading state and show user the error
      this.setState({ isLoading: false, formState: 'error', formUpdated: false, error: err.message });
    }

    return null;
  }

  /**
   * When user changes an input value, record that in the state.
   * Additionally, sets state that the form has been updated to 
   * allow for more fine validation control
   * 
   * @param {SyntheticEvent} cross-browser wrapper around the browser’s native event
   * 
   * @return {null}
   */
  _handleChange(e) {
    let state = { formUpdated: true };
    const input = e.target.name;
    const value = e.target.value;

    state[input] = value;

    this.setState(state);
  }
  //#endergion

  //#region React lifecycle events
  componentDidUpdate(prevProps){
    if(this.props.user.description !== prevProps.user.description){
      this.setState({description: this.props.user.description});
    }
  }

  render() {
    const { isLoading, error, formState, formUpdated, description, picture } = this.state;
    const { user } = this.props;
    const feedback = formState === 'success' ? 'Saved' : error;
    return (
      <Grid>
        <Row>
          <Col xs={12}>
            <PageHeader>Update { user.username } <small>{this.props.account}</small></PageHeader>
          </Col>
        </Row>
        <Row>
          <Col xs={12}>
            <form onSubmit={ isLoading || !formUpdated ? null : (e) => this._handleClick(e) }>
              <FieldGroup
                type="text"
                value={ user.username }
                disabled={true}
                name="username"
                label="Username"
              />
              <FieldGroup
                type="text"
                value={ description }
                placeholder="description"
                onChange={ (e) => this._handleChange(e) }
                name="description"
                label="Description"
                validationState={ formState }
              />
              <FieldGroup
                type="file"
                value={ picture }
                onChange={ (e) => this._handleChange(e) }
                name="picture"
                label="Profile picture"
                inputRef={ (input) => this.inputPicture = input }
                validationState={ formState }
              />
              <FormGroup>
                { user.picture.length ? <Image src={ user.picture } width="100" circle /> : '' }
              </FormGroup>
              <FormGroup>
                <Button
                  bsStyle="primary"
                  disabled={ isLoading || !formUpdated }
                  onClick={ isLoading || !formUpdated ? null : (e) => this._handleClick(e) }
                >
                  { isLoading ? 'Loading...' : 'Update profile' }
                </Button>
              </FormGroup>
              <FormGroup
                validationState={ formState }
              >
                <HelpBlock>{ feedback }</HelpBlock>
              </FormGroup>
            </form>
          </Col>
        </Row>
      </Grid>
    );
  }
  //#endregion
}

export default withRouter(UpdateUser);