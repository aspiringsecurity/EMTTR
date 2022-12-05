var walletURL = "http://127.0.0.1:8080/";
function doProcess(theUrl,callback){
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
        if (this.readyState == 4 && this.status == 200) {
           //console.log(xhttp.responseText);
           callback(null,xhttp.responseText);
        }
    };
    xhttp.open("GET", theUrl, true);
    xhttp.send();
}
function sendTransaction(accounToTransfer, amount, callback){
    var sendURL = `${walletURL}tx/sendTransaction?accountToTransfer=${accounToTransfer}&amount=${amount}`;
    doProcess(sendURL,(err,res)=>{
        if(!err){
            callback(null,res);
        }
    });
}
function contractFunctionTransaction(abi,contractAddress,contractFunction,callback){
    var sendURL = `${walletURL}tx/contractFunction?abi=${abi}&contractAddress=${contractAddress}&contractFunction=${contractFunction}`;
    doProcess(sendURL,(err,res)=>{
        if(!err){
            callback(null,res);
        }
    });
}
function contractFunctionTransactionCall(abi,contractAddress,contractFunction,callback){
    var sendURL = `${walletURL}tx/contractFunction/call?abi=${abi}&contractAddress=${contractAddress}&contractFunction=${contractFunction}`;
    doProcess(sendURL,(err,res)=>{
        if(!err){
            callback(null,res);
        }
    });
}
function checkWallet(callback){
    var sendURL = `${walletURL}testWallet`;
    doProcess(sendURL,(err,res)=>{
        callback(null,res);
    });
}