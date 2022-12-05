const Web3 = require('web3');

var web3 = new Web3();
web3.setProvider(new web3.providers.HttpProvider('http://localhost:8545'));

function callForValue(param,callback){
var abi = eval(param.abi);
var contractAddress = param.contractAddress;
  
var instance = new web3.eth.Contract(abi, contractAddress);
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

module.exports = {
    callForValue
}
