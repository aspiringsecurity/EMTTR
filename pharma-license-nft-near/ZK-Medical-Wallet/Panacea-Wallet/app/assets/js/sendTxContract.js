var Web3   = require('web3');

const EthereumTx = require('ethereumjs-tx');
// let privateKey;
// let accAddress;

function setKeys(accountToSet,privateKeyToSet){
    accAddress = accountToSet;
    privateKey = privateKeyToSet;
}
var Web3Utils = require('web3-utils');

var web3 = new Web3();
web3.setProvider(new web3.providers.HttpProvider('http://localhost:8545'));
// web3.setProvider(new web3.providers.HttpProvider('https://testnet2.matic.network'));

var Web3EthContract = require('web3-eth-contract');
Web3EthContract.setProvider('ws://localhost:8545');

function doInteractionWithSC(param,callback){
  // console.log(accAddress);
  var abi = eval(param.abi);
  var contractAddress = param.contractAddress;
  
  // var instance = new web3.eth.Contract(abi, contractAddress);
  var instance = new Web3EthContract(abi,contractAddress);

  var encodedCall = eval(param.encodedCall);
  var nonce, gasPrice;

  web3.eth.getTransactionCount(accAddress,(err,numberOfTxs) => {
    nonce = numberOfTxs;
    web3.eth.getGasPrice((err,price) => {
    //   console.log(price.toString(16));
    // gasPrice = Web3Utils.toBN(price);
    gasPrice  = price;
    // console.log(gasPrice.toString(16));
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

    web3.eth.sendRawTransaction(strTx, (err,transactionHash)=>{
      callback(transactionHash);
      console.log(transactionHash);
    })
  });
});
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
