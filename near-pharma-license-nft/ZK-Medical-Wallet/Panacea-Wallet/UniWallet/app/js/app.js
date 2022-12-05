let walletType;
let abi;
let contractAddress;
window.addEventListener('load',()=>{
    checkWallet((err,res)=>{
        walletType = res;
    });
})
let myInterval = setInterval(init,500);
function init(){
    clearInterval(myInterval);
    if(walletType){
        console.log(`Wallet Loaded: ${walletType}`);
    }else{
        console.log(`No Wallet Detected.`);
        console.log(`What the Fuck are you doing? Start UniWallet-> node index.js`);
    }
    abi = `[
        {
            "constant": false,
            "inputs": [
                {
                    "name": "_value",
                    "type": "uint256"
                }
            ],
            "name": "setValue",
            "outputs": [],
            "payable": false,
            "stateMutability": "nonpayable",
            "type": "function"
        },
        {
            "constant": true,
            "inputs": [],
            "name": "getValue",
            "outputs": [
                {
                    "name": "",
                    "type": "uint256"
                }
            ],
            "payable": false,
            "stateMutability": "view",
            "type": "function"
        }
    ]`;
    contractAddress = "0xe78a0f7e598cc8b0bb87894b0f60dd2a88d6a8ab";
}
function sendMoney(){
    console.log('Quickly See Node Wallet');
    let getAddrText = document.getElementById('addressText').value;
    let amount = document.getElementById('amount').value;
    sendTransaction(getAddrText, amount, (err,res)=>{
        if(!err){
            console.log(res);
        }
    });
}
function setValFunc(){
    console.log('Quickly See Node Wallet');
    let getVal = document.getElementById('setValueText').value;
    let cf = `setValue(${getVal})`;
    contractFunctionTransaction(abi,contractAddress,cf,(err,res)=>{
        if(!err){
           console.log(res);
        }
    })
}
function getValFunc(){
    let cf = `getValue()`;
    contractFunctionTransactionCall(abi,contractAddress,cf,(err,res)=>{
        if(!err){
           document.getElementById('getValueLabel').innerHTML = res;
        }
    })
}