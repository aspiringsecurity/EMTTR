const express = require('express')
const Web3 = require("web3");
const fs = require("fs");
const url = "http://localhost:8545";
const web3 = new Web3(new Web3.providers.HttpProvider(url));

AgeVerifier = require("./verify/build/contracts/AgeVerifier.json");
PasswordVerifier = require("./verify/build/contracts/PasswordVerifier.json");
VoteVerifier = require("./verify/build/contracts/VoteVerifier.json");
let ageVerifier, passwordVerifier, voteVerifier, accounts;


// const { generate_age_proof, generate_password_proof, get_hash } = require('./zklib')
const zklib = require('./zklib')
const app = express()

app.use(function(req, res, next) {
    res.header("Access-Control-Allow-Origin", "*");
    res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
    next();
  });

app.get('/age/proof/', async (req,res) => {

    let age = req.param("age");
    let agebar = req.param("agebar");
    let proof = await zklib.generate_age_proof(age, agebar);
    res.json(proof);
})

app.get('/password/hash/',async (req,res) => {

    let password = req.param("password");
    let hash = await zklib.get_hash(password);
    console.log(hash);
    res.json({hash:hash});
})

app.get('/password/proof/',async (req,res) => {

    let password = req.param("password");
    let password_hash = req.param("passwordhash");
    let proof = await zklib.generate_password_proof(password, password_hash);
    res.json(proof);

})

app.get('/vote/proof/',async (req,res) => {

    let secret = req.param("secret");
    let x1 = req.param("x1");
    let x2 = req.param("x2");
    let x3 = req.param("x3");
    let proof = await zklib.generate_vote_proof(secret, x1, x2, x3);
    res.json(proof);

})

app.get("/age/verify", async (req, res) => {

    let proof_string = req.param("ageproof");
    let age_proof = JSON.parse(proof_string);
    await getContracts();
    accounts = await web3.eth.getAccounts();
    console.log(accounts);
    var result = await ageVerifier.methods.verifyTx(age_proof["proof"]["a"], age_proof["proof"]["b"],age_proof["proof"]["c"],age_proof["inputs"])
    .call({from:accounts[0], gas:600000});
    console.log("result : ", result);
    res.json(result);
})


app.get("/password/verify", async (req, res) => {

    let proof_string = req.param("passproof");
    let password_proof = JSON.parse(proof_string);
    await getContracts();
    accounts = await web3.eth.getAccounts();
    console.log(accounts);
    var result  = await passwordVerifier.methods.verifyTx(password_proof["proof"]["a"],password_proof["proof"]["b"],password_proof["proof"]["c"],password_proof["inputs"])
    .call({from:accounts[0],gas:600000 });
    console.log(result);
    res.json(result);
})

app.get("/vote/verify", async (req, res) => {

    let proof_string = req.param("proof");
    let proof = JSON.parse(proof_string);
    await getContracts();
    accounts = await web3.eth.getAccounts();
    console.log(accounts);
    var result  = await voteVerifier.methods.verifyTx(proof["proof"]["a"],proof["proof"]["b"],proof["proof"]["c"],proof["inputs"])
    .call({from:accounts[0],gas:600000 });
    console.log(result);
    res.json(result);
})






async function getContracts() {
    console.log("Retrieving contract information...")
    let chainId = await web3.eth.net.getId()
    ageVerifier = new web3.eth.Contract(AgeVerifier.abi, AgeVerifier["networks"][chainId.toString()]["address"]);
    passwordVerifier = new web3.eth.Contract(PasswordVerifier.abi, PasswordVerifier["networks"][chainId.toString()]["address"]);
    voteVerifier = new web3.eth.Contract(VoteVerifier.abi, VoteVerifier["networks"][chainId.toString()]["address"]);
}


app.listen(3000, () =>{
    console.log(`Server started.`)
})

