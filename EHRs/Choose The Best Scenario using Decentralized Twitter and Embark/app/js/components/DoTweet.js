import { Link } from 'react-router-dom'
import { Button, FormGroup, ControlLabel, FormControl, HelpBlock } from 'react-bootstrap';
import React, { Component } from 'react';
import FieldGroup from './FieldGroup';

/**
 * Class that renders a form to allow the user to create
 * a tweet that is stored in the contract.
 * 
 * @extends React.Component
 */
class DoTweet extends Component{

  //#region Constructor
  constructor(props, context) {
    super(props, context);

    // initial state
    this.state = {
      tweet: '',
      tweetHasChanged: false,
      isLoading: false,
      error: ''
    };

    this.tweetInput = null;
  }
  //#endregion

  //#region Component events
  /**
   * Handles the 'Tweet' button click event which
   * sends a transaction to the contract to store a
   * tweet for the current user.
   * 
   * @returns {null}
   */
  _handleClick = async (e) => {

    // do not post tweet if there is a form error or user has not typed anything
    if(this._getValidationState() === 'error' || !this.state.tweetHasChanged){
      return e.preventDefault();
    }
    
    // show loading state
    this.setState({ isLoading: true });

    const { username, account, onAfterTweet } = this.props;
    const tweet = DTwitter.methods.tweet(this.state.tweet);
    
    try{
      // estimate gas before sending tweet transaction
      const gasEstimate = await tweet.estimateGas({ from: web3.eth.defaultAccount });
      // send the tweet transaction plus a little extra gas in case the contract state
      // has changed since we've done our gas estimate
      await tweet.send({ from: web3.eth.defaultAccount, gas: gasEstimate + 1000 });
      // remove loading state
      this.setState({ isLoading: false });

      // tell parent we've updated a user and to re-fetch user details from the contract
      onAfterTweet();
    }
    catch(err){
      // remove loading state and show error message
      this.setState({ isLoading: false, error: err.message });
    }
  }

   /**
   * When user changes an input value, record that in the state.
   * 
   * @param {SyntheticEvent} cross-browser wrapper around the browser’s native event
   * 
   * @return {null}
   */
  _handleChange(e) {
    let state = {tweetHasChanged: true};
    state[e.target.name] = e.target.value;
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
    return ((this.state.tweet === '' && !this.state.tweetHasChanged) || (this.state.tweet.length > 0 && this.state.tweet.length <= 140)) ? null : 'error';
  }
  //#endregion

  //#region React lifecycle events
  componentDidMount(){
    // set focus to tweet textarea after render
    if(this.tweetInput) this.tweetInput.focus();
  }

  render(){

    const validationState = this._getValidationState();
    const isValid = validationState !== 'error';
    const { isLoading, error, tweet, tweetHasChanged } = this.state;

    let feedback = !isValid ? 'Tweet must be 140 characters or less' : '';
    if(this.state.error) feedback = error;

    return (
      <form>
        <FieldGroup
          type="text"
          value={ tweet }
          placeholder="140 characters or less..."
          onChange={ (e) => this._handleChange(e) }
          name="tweet"
          componentClass="textarea"
          hasFeedback={true}
          validationState={validationState}
          inputRef={(input) => { this.tweetInput = input; }}
        />
        <Button
          bsStyle="primary"
          disabled={ !isValid || Boolean(error) || !tweetHasChanged }            
          onClick={ (!isValid || Boolean(error) || !tweetHasChanged) ? null : (e) => this._handleClick(e) }
        >{isLoading ? 'Loading...' : 'Post tweet'}</Button>
        <FormGroup
          controlId="formBasicText"
          validationState={ validationState }
        >
          <HelpBlock>{ feedback }</HelpBlock>
        </FormGroup>
      </form>
    );
  }
  //#endregion
}
export default DoTweet