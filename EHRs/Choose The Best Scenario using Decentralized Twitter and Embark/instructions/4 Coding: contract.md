## Coding: DTwitter contract
We can start updating our `/contracts/DTwitter.sol` contract code and watch as Embark watches and recompiles everything for us as we go.
### Functions
Let’s begin by writing some of our contracts functions
###### Create account
The `createAccount` function will be used for creating users in the contract. We need to add the new user to the user mapping and keep track of the address that created the user.
1. Add a user to the users mapping and populate details (creationDate, owner, username, description)
    ```
    users[usernameHash].creationDate = now;
    users[usernameHash].owner = msg.sender;
    users[usernameHash].username = username;
    users[usernameHash].description = description;
    ```
2. Add entry to our owners mapping so we can retrieve user by their address
    ```
    owners[msg.sender] = usernameHash;
    ```
###### Edit account
We will be updating the user's profile in the `editAccount` function, so we need to update the user's description and picture.
1. Update the description (could be empty)
    ```
    users[usernameHash].description = description;
    ```
2. Only update the user's picture if the hash passed in is not empty or null        (essentially disallows deletions)
    ```
    if (bytes(pictureHash).length > 0) {
       users[usernameHash].picture = pictureHash;
    }
    ```
###### User exists
The `userExists` function is used for checking if a user exists in the `users` mapping stored in the contract.
1. This can be achieved by checking a property on a specific item in the `users` mapping
    ```
    return users[usernameHash].creationDate != 0;
    ```
###### Tweet
For the `tweet` function to work, we need to get the user associated with the `msg.sender`, store the tweet in the user mapping, then emit an event that a tweet occurred. Later, we will subscribe to this event to get tweets as they are added to the contract.
1. Get our user from the usernameHash
    ```
    User storage user = users[usernameHash];
    ```
2. Get our new tweet index
    ```
    uint tweetIndex = user.tweets.length++;
    ```
3. Update the user's tweets at the tweet index
    ```
    user.tweets[tweetIndex] = content;
    ```
4. Emit the tweet event and notify the listeners
    ```
    emit NewTweet(usernameHash, content, now);
    ```