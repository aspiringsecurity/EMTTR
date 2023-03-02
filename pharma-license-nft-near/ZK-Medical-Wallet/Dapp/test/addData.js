const Election = require("../build/contracts/Election.json");
// const assert = require("chai").assert;
// const truffleAssert = require('truffle-assertions');
const Web3 = require('web3')
var web3 = new Web3(new Web3.providers.HttpProvider("http://localhost:8545/"));

var ec_head, ec_official, kyc_verifier;
var ElectionInstance;
var voter1, voter2, voter3;
var vote_hash1, vote_hash2, vote_hash3;



async function getAccounts(){

    var accs = await web3.eth.getAccounts();
    //console.log(accs);
    ec_head = accs[0];
    ec_official = accs[1];
    var voter1 = accs[3];
    var uuid1 = 121212121212;
    var security_token1 = "abcde";
    var uuid_hash_1 = web3.utils.keccak256(uuid1.toString());
    var voter2 = accs[4];
    var uuid2 = 232323232323;
    var security_token2 = "12345";
    var uuid_hash_2 = web3.utils.keccak256(uuid2.toString());
    var voter3 = accs[5];
    var uuid3 = 343434343434;
    var security_token3 = "qwerty";
    var uuid_hash_3 = web3.utils.keccak256(uuid3.toString());
    ElectionInstance = await new web3.eth.Contract(Election.abi, '0x47eCB5374B46e6645E5A1BB076219C070B7A69ce');
    ElectionInstance.methods.addConstituency("roorkee").send({from:ec_head})
    .then((receipt) => {
        console.log(receipt);
        // receipt can also be a new contract instance, when coming from a "contract.deploy({...}).send()"
    });
    await ElectionInstance.methods.addParty("bjp").send({ from: ec_head });
    await ElectionInstance.methods.addParty("congress").send({ from: ec_head });
    await ElectionInstance.methods.addParty("bsp").send({ from: ec_head });

    var const0 = await ElectionInstance.methods.constituencies(0).call();
    var const1 = await ElectionInstance.methods.constituencies(1).call();
    var party1 = await ElectionInstance.methods.parties(0).call();
    var party2 = await ElectionInstance.methods.parties(1).call();
    var party3 = await ElectionInstance.methods.parties(2).call();

    vote_hash1 = web3.utils.keccak256(uuid1.toString() + security_token1);
    vote_hash2 = web3.utils.keccak256(uuid2.toString() + security_token2);
    vote_hash3 = web3.utils.keccak256(uuid3.toString() + security_token3);

    // await ElectionInstance.methods.kycVerify(uuid_hash_1).send({from: ec_head});
    // await ElectionInstance.methods.kycVerify(uuid_hash_2).send({from: ec_head});
    // await ElectionInstance.methods.kycVerify(uuid_hash_3).send({from: ec_head});

    // console.log(await web3.eth.getBalance(accs[2]));

    await ElectionInstance.methods.registerVote(uuid_hash_1, "roorkee", "congress", vote_hash1).send({ from: voter1, gas:3000000 });
    await ElectionInstance.methods.registerVote(uuid_hash_2, "roorkee", "congress", vote_hash2).send({ from: voter2, gas:3000000 });
    await ElectionInstance.methods.registerVote(uuid_hash_3, "roorkee", "bsp", vote_hash3).send({ from: voter3, gas:3000000 });

    var voterCount = await ElectionInstance.methods.voterCount.call();
    // console.log(voterCount);

    await ElectionInstance.methods.calculateVotes.call()
    var roorkee_bjp = await ElectionInstance.methods.getVoteCount.call("roorkee", "bjp", { from: voter1 });
    var roorkee_bsp = await ElectionInstance.methods.getVoteCount.call("roorkee", "bsp", { from: voter1 });
    var roorkee_congress = await ElectionInstance.methods.getVoteCount.call("roorkee", "congress", { from: voter1 });

    var winner_roorkee = await ElectionInstance.methods.getWinner.call("roorkee", { from: ec_head });

    var roorkee_congress_votes = await ElectionInstance.methods.getVoteHashes.call("roorkee", "congress", { from: voter1 });
    var roorkee_bsp_votes = await ElectionInstance.methods.getVoteHashes.call("roorkee", "bsp", { from: voter1 });
    var roorkee_bjp_votes = await ElectionInstance.methods.getVoteHashes.call("roorkee", "bjp", { from: voter1 });
    console.log(roorkee_bjp_votes);
}

 getAccounts();

// async function addConsti(){
//     // console.log("here ", ElectionInstance);


//     var echead = await ElectionInstance.methods.EC_Head().call();
//     console.log(ec_head);
    

    // await ElectionInstance.methods.addParty("bjp", { from: ec_official });
    // await ElectionInstance.methods.addParty("congress", { from: ec_official });
    // await ElectionInstance.methods.addParty("bsp", { from: ec_official });


    // var const0 = await ElectionInstance.methods.constituencies.call(0);
    // var const1 = await ElectionInstance.methods.constituencies.call(1);
    // var party1 = await ElectionInstance.methods.parties.call(0);
    // var party2 = await ElectionInstance.methods.parties.call(1);
    // var party3 = await ElectionInstance.methods.parties.call(2);

    // vote_hash1 = web3.utils.keccak256(uuid1.toString() + security_token1);
    // vote_hash2 = web3.utils.keccak256(uuid2.toString() + security_token2);
    // vote_hash3 = web3.utils.keccak256(uuid3.toString() + security_token3);

    // await ElectionInstance.methods.registerVote(uuid_hash_1, "roorkee", "congress", vote_hash1, { from: voter1 });
    // await ElectionInstance.methods.registerVote(uuid_hash_2, "roorkee", "congress", vote_hash2, { from: voter2 });
    // await ElectionInstance.methods.registerVote(uuid_hash_3, "roorkee", "bsp", vote_hash3, { from: voter3 });

    // var voterCount = await ElectionInstance.methods.voterCount.call();

    // await ElectionInstance.methods.calculateVotes({ from: ec_head });
    // var roorkee_bjp = await ElectionInstance.methods.getVoteCount.call("roorkee", "bjp");
    // var roorkee_bsp = await ElectionInstance.methods.getVoteCount.call("roorkee", "bsp", { from: voter1 });
    // var roorkee_congress = await ElectionInstance.methods.getVoteCount.call("roorkee", "congress", { from: voter1 });

    // var winner_roorkee = await ElectionInstance.methods.getWinner.call("roorkee", { from: ec_head });

    // var roorkee_congress_votes = await ElectionInstance.methods.getVoteHashes.call("roorkee", "congress");
    // var roorkee_bsp_votes = await ElectionInstance.methods.getVoteHashes.call("roorkee", "bsp");
    // var roorkee_bjp_votes = await ElectionInstance.methods.getVoteHashes.call("roorkee", "bjp");
// }

// addConsti();