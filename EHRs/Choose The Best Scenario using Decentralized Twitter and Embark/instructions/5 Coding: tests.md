## Contract tests
Now that we have our contract written, we can use the code generation provided by EmbarkJS to write our contract unit test cases for TDD development. Embark will code generate a DTwitter javascript object from our contract and make it accessible in our dApp by way of the `DTwitter` object. 
> Additionally, Embark also has an API (`EmbarkJS`) for decentralised storage (IPFS and Swarm), and decentralised communication (Whisper), which are all configurable in config files. 
###### Create account transaction should be successful
Our first test is to ensure that our `createAccount` transaction is sent successfully.
```
// do the create account tx
const createAccountTx = await createAccount(username, description).send();
```
###### Should create user
After our successful `createAccount` transaction, this should have created a user. 
```
// Get user details from contract
const user = await users(web3.utils.keccak256(username)).call();
```
###### Should create owner for default account
The `createAccount` function should have also created an entry in the `owners` mapping with our `defaultAccount`
```
// read from the owners mapping the value associated with the defaultAccount
const usernameHash = await owners(web3.eth.defaultAccount).call();
```
###### User exists should be true
We can use our `userExists` function to check if a usernameHash exists. This will later be used for validation
```
// Check the usernamehash exists
const exists = await userExists(usernameHash).call();
```
###### Edit Account should update user details
After a call `editAccount`, our user's details should be updated.
1. Call edit account
    ```
    await editAccount(usernameHash, updatedDescription, updatedImageHash).send();
    ```
2. Then fetch the user details with the usernamehash
    ```
    const updatedUserDetails = await users(usernameHash).call();
    ```
###### Tweet event should fire when there is a tweet
We need to ensure that our contract events subscription works correctly when someone creates a new tweet via the `tweet` function.
1. Subscribe to the `NewTweet` event
    ```
    DTwitter.events.NewTweet({
        filter: { _from: usernameHash },
        fromBlock: 0
   })
   .on('data', (event) => {
        assert.equal(event.returnValues.tweet, tweetContent);
    });
    ```
2. Do the tweet
    ```
    await tweet(tweetContent).send();
    ```
### Run tests
Let's run the tests to ensure they are all passing. In a new terminal, run:
```
embark test
```
###### Test results
Your test results should appear similar to the following:
```
DTwitter contract
    ✓ transaction to create a dtwitter user 'testhandle' with description 'test description' should be successful (114ms)
    ✓ should have created a user 'testhandle' (60ms)
    ✓ should have created an owner for our defaultAccount
    ✓ should know 'testhandle' exists (49ms)
    ✓ should be able to edit 'testhandle' user details (186ms)
    ✓ should be able to add a tweet as 'testhandle' and receive it via contract event (54ms)
```
> Note: there is a known error with `ganache` that sometimes causes last test does to fail with the error `ERROR: The returned value is not a convertible string`. We are currently investigating this issue.
