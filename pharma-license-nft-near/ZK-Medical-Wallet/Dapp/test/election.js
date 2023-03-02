const Election = artifacts.require("Election");
const assert = require("chai").assert;
const truffleAssert = require('truffle-assertions');
const Web3 = require('web3')
var web3 = new Web3();
contract('Election', (accounts) => {

    var ec_head, ec_official, kyc_verifier;
    var ElectionInstance;
    var voter1, voter2, voter3;
    var vote_hash1, vote_hash2, vote_hash3;
    // // build up and tear down a new Casino contract before each test

    // beforeEach(async () => {
    //     meta = await Election.new({ from: fundingAccount });
    //     await meta.fund({ from: fundingAccount, value: fundingSize });
    //     assert.equal(web3.eth.getBalance(meta.address).toNumber(), fundingSize);
    // });

    // afterEach(async () => {
    //     await meta.kill({ from: fundingAccount });
    // });

    before(async () => {
        ElectionInstance = await Election.deployed();
        // Setup 2 accounts.
        ec_head = accounts[0];
        ec_official = accounts[1];
        kyc_verifier = accounts[2];
        voter1 = accounts[3];
        uuid1 = 121212121212;
        security_token1 = "abcde";
        uuid_hash_1 = web3.utils.keccak256(uuid1.toString());
        voter2 = accounts[4];
        uuid2 = 232323232323;
        security_token2 = "12345";
        uuid_hash_2 = web3.utils.keccak256(uuid2.toString());
        voter3 = accounts[5];
        uuid3 = 343434343434;
        security_token3 = "qwerty";
        uuid_hash_3 = web3.utils.keccak256(uuid3.toString());
    })

    describe("success states", async () => {

        it('should add ec official correctly', async () => {
            // const ElectionInstance = await Election.deployed();

            const EC_Head = await ElectionInstance.EC_Head.call();
            assert.equal(EC_Head, ec_head, "the ec head is not set correctly");
            await ElectionInstance.addEcOfficial(ec_official, { from: ec_head });
            var is_official = await ElectionInstance.ec_officials.call(ec_official);
            assert.equal(is_official, true, "EC official not set correctly");
        });
        // it('should add kyc verifier correctly', async () => {
        //     // const ElectionInstance = await Election.deployed();

        //     const is_official = await ElectionInstance.ec_officials.call(ec_official);
        //     assert.equal(is_official, true, "EC official not set correctly");
        //     await ElectionInstance.addKycVerifier(kyc_verifier, { from: ec_official });
        //     var is_verifier = await ElectionInstance.kyc_verifiers.call(kyc_verifier);
        //     assert.equal(is_verifier, true, "kyc verifier not set correctly");
        // });
        // it('should do kyc verification correctly', async () => {

        //     await ElectionInstance.kycVerify(uuid_hash_1, { from: kyc_verifier });
        //     await ElectionInstance.kycVerify(uuid_hash_2, { from: kyc_verifier });
        //     await ElectionInstance.kycVerify(uuid_hash_3, { from: kyc_verifier });

        //     var is_verified_1 = await ElectionInstance.kycDone.call(uuid_hash_1);
        //     var is_verified_2 = await ElectionInstance.kycDone.call(uuid_hash_2);
        //     var is_verified_3 = await ElectionInstance.kycDone.call(uuid_hash_3);

        //     assert.equal(is_verified_1, true, "kyc verification not done correctly");
        //     assert.equal(is_verified_2, true, "kyc verification not done correctly");
        //     assert.equal(is_verified_3, true, "kyc verification not done correctly");
        // });
        it('should add constituency and party correctly', async () => {

            await ElectionInstance.addConstituency("banglore", { from: ec_official });
            await ElectionInstance.addConstituency("roorkee", { from: ec_official });

            await ElectionInstance.addParty("bjp", { from: ec_official });
            await ElectionInstance.addParty("congress", { from: ec_official });
            await ElectionInstance.addParty("bsp", { from: ec_official });


            var const0 = await ElectionInstance.constituencies.call(0);
            var const1 = await ElectionInstance.constituencies.call(1);
            var party1 = await ElectionInstance.parties.call(0);
            var party2 = await ElectionInstance.parties.call(1);
            var party3 = await ElectionInstance.parties.call(2);

            assert.equal(const0, "banglore", "constituency not set correctly");
            assert.equal(const1, "roorkee", "constituency not set correctly");
            assert.equal(party1, "bjp", "party not set correctly");
            assert.equal(party2, "congress", "party not set correctly");
            assert.equal(party3, "bsp", "party not set correctly");
        });

        it('should register vote correctly', async () => {

            vote_hash1 = web3.utils.keccak256(uuid1.toString() + security_token1);
            vote_hash2 = web3.utils.keccak256(uuid2.toString() + security_token2);
            vote_hash3 = web3.utils.keccak256(uuid3.toString() + security_token3);

            await ElectionInstance.registerVote(uuid_hash_1, "roorkee", "congress", vote_hash1, { from: voter1 });
            await ElectionInstance.registerVote(uuid_hash_2, "roorkee", "congress", vote_hash2, { from: voter2 });
            await ElectionInstance.registerVote(uuid_hash_3, "roorkee", "bsp", vote_hash3, { from: voter3 });

            var voterCount = await ElectionInstance.voterCount.call();
            assert.equal(voterCount, 3, "votes not registered correctly");

        });

        it('should calculate votes and declare winner correctly', async () => {

            await ElectionInstance.calculateVotes({ from: ec_head });
            var roorkee_bjp = await ElectionInstance.getVoteCount.call("roorkee", "bjp");
            var roorkee_bsp = await ElectionInstance.getVoteCount.call("roorkee", "bsp", { from: voter1 });
            var roorkee_congress = await ElectionInstance.getVoteCount.call("roorkee", "congress", { from: voter1 });

            assert.equal(roorkee_bjp, 0, "result not set correctly");
            assert.equal(roorkee_bsp, 1, "result not set correctly");
            assert.equal(roorkee_congress, 2, "result not set correctly");

            var winner_roorkee = await ElectionInstance.getWinner.call("roorkee", { from: ec_head });
            assert.equal(winner_roorkee, "congress", "winner not set correctly");

            var roorkee_congress_votes = await ElectionInstance.getVoteHashes.call("roorkee", "congress");
            var roorkee_bsp_votes = await ElectionInstance.getVoteHashes.call("roorkee", "bsp");
            var roorkee_bjp_votes = await ElectionInstance.getVoteHashes.call("roorkee", "bjp");

            console.log("roorkee congress : ", roorkee_congress_votes);
            console.log("roorkee bsp : ", roorkee_bsp_votes);
            console.log("roorkee bjp : ", roorkee_bjp_votes);

            assert.equal(roorkee_congress_votes[0], vote_hash1, "vote hashes not set correctly");
            assert.equal(roorkee_congress_votes[1], vote_hash2, "vote hashes not set correctly");
            assert.equal(roorkee_bsp_votes[0], vote_hash3, "vote hashes not set correctly");
            assert.equal(roorkee_bjp_votes.length, 0, "vote hashes not set correctly");

        });

    })
    describe("failure states", async () => {
        // it("should not send or execute the function if not enough funds", async () => {

        //     var amount = 10;
        //     await truffleAssert.reverts(ElectionInstance.sendCoin(accountOne, amount, { from: accountTwo }), "insufficient funds");
        // });
    })

});
