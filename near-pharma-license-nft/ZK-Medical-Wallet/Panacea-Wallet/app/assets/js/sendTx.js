
var  Web3 = require('web3');
const ethereumjs = require('ethereumjs-tx');
// var account = "0x90f8bf6a479f320ead074411a4b0e7944ea8c9c1";
// var privateKey = "4f3edf983ac636a65a842ce7c78d9aa706d3b113bce9c46f30d7d21715b23b1d";
var tHash;
var account;
var privateKey;
function setKeys(accountToSet,privateKeyToSet){
    account = accountToSet;
    privateKey = privateKeyToSet;
}

 web3 = new Web3(new Web3.providers.HttpProvider("http://localhost:8545"));
function sendTransaction(accountToTransfer, amount, callback){
  web3.eth.getTransactionCount(account, function (err, nonce) {
    // var data = web3.eth.contract(abi).at(address).increment.getData();
    var txParams = {
        nonce: nonce,
        gasPrice: 1,
        gasLimit: 1000000,
        to: accountToTransfer,
        value: amount * 1000000000000000000
      };
    var tx = new ethereumjs(txParams);
    tx.sign(Buffer.from(privateKey, 'hex'));

    var raw = '0x' + tx.serialize().toString('hex');
    web3.eth.sendRawTransaction(raw, function (err, transactionHash) {
      //console.log(transactionHash);
      if(err){
          console.log(err);
      }
      tHash =  transactionHash;
      callback(transactionHash);
    });
  });
  
}

// module.exports = {
//     sendTransaction,
//     setKeys
// }