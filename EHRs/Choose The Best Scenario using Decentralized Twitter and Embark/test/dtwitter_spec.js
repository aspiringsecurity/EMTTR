// our DTwitter contract object to test
const DTwitter = require('Embark/contracts/DTwitter');

// contract methods we'll be testing
const { createAccount, users, owners, userExists, editAccount, tweet } = DTwitter.methods;

// variables that will be updated in the tests
let accounts;

// set up our config test parameters
config({
  contracts: {
    DTwitter: {
      // would pass constructor args here if needed
    }
  }
}, (err, theAccounts) => {
  // this is the list of accounts our node / wallet controls.
  accounts = theAccounts;
});

// other test parameters
const username = 'testhandle';
const description = 'test description';
const tweetContent = 'test tweet';

// Embark exposes a global contract method as an alias
// for Mocha.describe
contract("DTwitter contract", function () {
  this.timeout(0);


  // it(`should create a dtwitter user '${username}' with description '${description}'`, async function () {
  it("transaction to create a dtwitter user 'testhandle' with description 'test description' should be successful", async function () {

    // do the create account
    const createAccountTx = await createAccount(username, description).send();

    // assert that the transaction was successful
    assert.equal(createAccountTx.status, true);

  });

  it("should have created a user 'testhandle'", async function () {

    // get user details from contract
    const user = await users(web3.utils.keccak256(username)).call();

    assert.equal(user.username, username);
    assert.equal(user.description, description);

  });

  it("should have created an owner for our defaultAccount", async function () {
    
    // read from the owners mapping
    const usernameHash = await owners(web3.eth.defaultAccount).call();

    // check the return value from owners mapping matches
    assert.equal(usernameHash, web3.utils.keccak256(username));
  });

  it("should know 'testhandle' exists", async function () {
    const usernameHash = web3.utils.keccak256(username);
    const exists = await userExists(usernameHash).call();

    assert.equal(exists, true);
  });


  it("should be able to edit 'testhandle' user details", async function () {
    const usernameHash = web3.utils.keccak256(username);
    const updatedDescription = description + ' edited';
    const updatedImageHash = 'QmWvPtv2xVGgdV12cezG7iCQ4hQ52e4ptmFFnBK3gTjnec';

    await editAccount(usernameHash, updatedDescription, updatedImageHash).send();

    const updatedUserDetails = await users(usernameHash).call();

    assert.equal(updatedUserDetails.description, updatedDescription);
    assert.equal(updatedUserDetails.picture, updatedImageHash);
  });

  it("should be able to add a tweet as 'testhandle' and receive it via contract event", async function () {
    const usernameHash = web3.utils.keccak256(username);
    
    await tweet(tweetContent).send();

    DTwitter.events.NewTweet({
      filter: { _from: usernameHash },
      fromBlock: 1
    })
    .on('data', (event) => {
      assert.equal(event.returnValues.tweet, tweetContent);
    });
  });

});
