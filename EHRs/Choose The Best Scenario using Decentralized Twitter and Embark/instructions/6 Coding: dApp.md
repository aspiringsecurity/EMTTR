## Coding: dApp
Let’s use the code-generated `DTwitter` API and the `EmbarkJS` API to interact with our contact in our dApp. As we update the .js files, notice how Embark watches the files and compiles as we save.
###### `CreateUser.js`
1. We need to update the `_handleChange` function that will check if a user exists as the user types in a username. 
```
    // call the userExists method in our contract asynchronously
    DTwitter.methods.userExists(web3.utils.keccak256(value)).call()
```
2. We need to update the `_handleClick` event that is fired when the 'Create user' button is clicked. This should handle the creation of the user in the contract. We need to get a gas estimate and call our contract's `createAccount` function
    ```
    // set up our contract method with the input values from the form
    const createAccount = DTwitter.methods.createAccount(username, description);

     // get a gas estimate before sending the transaction
     const gasEstimate = await createAccount.estimateGas({ from: web3.eth.defaultAccount, gas: 10000000000 });

     // send the transaction to create an account with our gas estimate
     // (plus a little bit more in case the contract state has changed).
     const result = await createAccount.send({ from: web3.eth.defaultAccount,  gas: gasEstimate + 1000 });
    ```
###### `UpdateUser.js`
The `UpdateUser` component will call our `editAccount` contract function to update the details of the user. This happens when the 'Update profile' button is clicked and the `_handleClick` event is fired. In the `_handleClick` event:
```
// upload the file to ipfs and get the resulting hash
await EmbarkJS.Storage.uploadFile([this.inputPicture]);

// get a handle for the editAccount method
const editAccount = DTwitter.methods.editAccount(web3.utils.keccak256(user.username), description, hash);

// get a gas estimate for the transaction with the input username and description
const gasEstimate = await editAccount.estimateGas({ from: web3.eth.defaultAccount, gas: 10000000000 });

// send the transaction with our gas estimate (plus a little bit more in case the contract) state has changed since we got our estimate
const result = await editAccount.send({ from: web3.eth.defaultAccount, gas: gasEstimate + 1000 });
```
###### `DoTweet.js`
The DoTweet components sends a tweet to the contract. We will need to update the  `_handleClick` even to estimate gas and call the contract's `tweet` function.
```
// estimate gas before sending tweet transaction
const gasEstimate = await tweet.estimateGas({ from: web3.eth.defaultAccount, gas: 10000000000 });
    
// send the tweet transaction plus a little extra gas in case the contract state
// has changed since we've done our gas estimate
await tweet.send({ from: web3.eth.defaultAccount, gas: gasEstimate + 1000 });
```
###### `UserTweets.js`
The `UserTweets` component is used to 1.) show the the profile of the user who's tweets we'd like to view, and 2.) show the tweets of the user. 
1. In `_getUserDetails`, we need to get the details of the user
    ```
    //get user details and update state
    let user = await DTwitter.methods.users(web3.utils.keccak256(username)).call();

    // update picture url for ipfs
    user.picture = user.picture.length > 0 ? EmbarkJS.Storage.getUrl(user.picture) : imgAvatar;
    ```
2. In the `_subscribeToNewTweetEvent`, we will subscribe to the `NewTweet` contract event. Replace `new EventEmitter()` with 
```
DTwitter.events.NewTweet({
   filter: {_from: web3.utils.keccak256(username)},
   fromBlock: 1
 }, (err, event) => {
   if (err){
     this.props.onError(err, 'UserTweets._subscribeToNewTweetEvent');
   }
})
```
#### Run through the site and fix any errors
Let's try out the functionality on the site and fix any errors we may have introduced.
