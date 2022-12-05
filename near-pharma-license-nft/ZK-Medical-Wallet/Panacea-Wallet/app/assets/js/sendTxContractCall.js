// var Web3 = require('web3');
var Web3EthContract = require('web3-eth-contract');
Web3EthContract.setProvider('ws://localhost:8545');

var web3 = new Web3();
web3.setProvider(new web3.providers.HttpProvider('http://localhost:8545'));
// web3.setProvider(new web3.providers.HttpProvider('https://testnet2.matic.network'));

function callForValue(param,callback){
var abi = eval(param.abi);
var contractAddress = param.contractAddress;
  
var instance = new Web3EthContract(abi,contractAddress);

// var instance = new web3.eth.Contract(abi, contractAddress);
// var instance = web3.eth.contract(abi, contractAddress);
// console.log(instance);
var test;
var callVal = eval(param.encodedCall);
callVal.call(function(err, result) {
    if(!err){
        //console.log(result);
        test = result;
        callback(null,test);
    }
});
}
