const Web3 = require("web3");
// require("dotenv").config();
const fs = require("fs");
const url = "http://localhost:8545"
const web3 = new Web3(new Web3.providers.HttpProvider(url));

Election = require("./build/contracts/Election.json");
// VoterData = require("./build/contracts/VoterData.json");
let election;

async function getContracts() {
    console.log("Retrieving contract information...")
    let chainId = await web3.eth.net.getId()
    election = new web3.eth.Contract(Election.abi, Election["networks"][chainId.toString()]["address"])
}

let account = {
    address: "0xc7031b6afa7972560618ba7ccf2ae099ae5c393f",
    privateKey: "63189f25e57c6c25f9a619a364094792e78f788209e276cd2d38951fcedec9ac"
}

let accounts;

async function setupElection() {

    await getContracts();
    accounts = await web3.eth.getAccounts();
    console.log(accounts);
    var parties = ["BJP", "Congress", "BSP"];
    var constituencies = ["mumbai"];
    console.log(election);
    var tx_hash = await election.methods.addElectionDetails(constituencies, parties).send({
      from:accounts[0],gas:600000 
    }).on("receipt", receipt => {
      console.log("added");
      console.log(receipt);
    });

    let constituencies_stored = await election.methods.getConstituencies().call();
    console.log("constituencies stored : ", constituencies_stored);
    let parties_stored = await election.methods.getParties().call();
    console.log("parties stored : ", parties_stored);
    // var tx_hash = await election.methods.addConstituency("mumbai").send({
    //     from:accounts[0]
    //   }).on("receipt", receipt => {
    //     console.log("added");
    //     console.log(receipt);
    //   });
    //   var tx_hash = await election.methods.addParty("BJP").send({
    //     from:accounts[0]
    //   }).on("receipt", receipt => {
    //     console.log("added");
    //     console.log(receipt);
    //   });
    //   var tx_hash = await election.methods.addParty("Congress").send({
    //     from:accounts[0]
    //   }).on("receipt", receipt => {
    //     console.log("added");
    //     console.log(receipt);
    //   });
    //   var tx_hash = await election.methods.addParty("BSP").send({
    //     from:accounts[0]
    //   }).on("receipt", receipt => {
    //     console.log("added");
    //     console.log(receipt);
    //   });
    
}

setupElection()
