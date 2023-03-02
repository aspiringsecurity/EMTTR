const Web3   = require('web3');

const EthereumTx = require('ethereumjs-tx');
let privateKey;
let accAddress;

function setKeys(accountToSet,privateKeyToSet){
    accAddress = accountToSet;
    privateKey = privateKeyToSet;
}

var web3 = new Web3();
web3.setProvider(new web3.providers.HttpProvider('http://localhost:8545'));

function doInteractionWithSC(param){

  var abi = eval(param.abi);
  var contractAddress = param.contractAddress;
  
  var instance = new web3.eth.Contract(abi, contractAddress);
  
  var encodedCall = eval(param.encodedCall);
  var nonce, gasPrice;

  web3.eth.getTransactionCount(accAddress)
  .then((numberOfTxs) => {
    nonce = numberOfTxs;
    return web3.eth.getGasPrice();
  })
  .then((price) => {
    gasPrice = web3.utils.toBN(price);
    var gasLimit = 200000;
    var txParams = {
      nonce:    '0x' + nonce.toString(16),
      gasPrice: '0x' + gasPrice.toString(16),
      gasLimit: '0x' + gasLimit.toString(16),
      data:            encodedCall,
      to:              contractAddress
    };

    var tx = new EthereumTx(txParams);
    tx.sign(Buffer.from(privateKey, 'hex'));

    var strTx = '0x' + tx.serialize().toString('hex'); // PAY CLOSE ATENTION TO THE '0x'!!!!!

    web3.eth.sendSignedTransaction(strTx)
    .once('transactionHash', function(txid) {
      console.log('\n\ttxid: ' + txid + '\n');
    })
    .catch((ex) => {
      console.log(ex);
    })
  })
  .catch((ex) => {
    console.log(ex);
  })
}

// var param = {
//     abi:[
//         {
//             "constant": false,
//             "inputs": [
//                 {
//                     "name": "_value",
//                     "type": "uint256"
//                 }
//             ],
//             "name": "setValue",
//             "outputs": [],
//             "payable": false,
//             "stateMutability": "nonpayable",
//             "type": "function"
//         },
//         {
//             "constant": true,
//             "inputs": [],
//             "name": "getValue",
//             "outputs": [
//                 {
//                     "name": "",
//                     "type": "uint256"
//                 }
//             ],
//             "payable": false,
//             "stateMutability": "view",
//             "type": "function"
//         }
//     ],
//     contractAddress: "0xe78a0f7e598cc8b0bb87894b0f60dd2a88d6a8ab",
//     encodedCall: "instance.methods.setValue(40).encodeABI()"
// }
module.exports = {
    doInteractionWithSC,
    setKeys
}