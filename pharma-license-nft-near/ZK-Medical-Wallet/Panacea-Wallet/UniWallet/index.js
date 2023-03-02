console.log('Welcome To UniWallet v1.0');

const express = require('express');
const readline = require('readline');


const walletVault= require('./wallet-vault');
const sendTx = require('./sendTx');
const sendTxContract = require('./sendTxContract')
const sendTxContractCall = require('./sendTxContractCall')

const app = express();
const port = 8080;

let privKeyData = walletVault.privKeyData();
let keyIndex = 0;

const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
});


// routes will go here
app.use(function(req, res, next) {
    res.setHeader("Access-Control-Allow-Origin", "*");
    // Request headers you wish to allow
    res.setHeader("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
    // Set to true if you need the website to include cookies in the requests sent
    res.setHeader('Access-Control-Allow-Credentials', true);
    // Pass to next layer of middleware
    next();
});

app.get('/testWallet', function(req, res) {
    res.send('UniWallet v1.0');
});

app.get('/tx/sendTransaction',(req,res)=>{
    try{
        sendTx.setKeys(privKeyData[keyIndex].addr,privKeyData[keyIndex].privkey);
        fs.writeFileSync('app/trans.txt', req.query.val);
        openWindow();
        childWindow.on('closed', () => {
            win = null
            let s = fs.readFileSync('app/resp.txt');
            
            if(s === 'y'){
                if(req.param('accountToTransfer')){
                    let t = sendTx.sendTransaction(req.param('accountToTransfer'));
                    res.send("Transaction Signed Successfully");
                    console.log('Transaction Signed Successfully');
                }else{
                    res.send("Something went wrong!")
                }
            }else{
                console.log('Transaction Declined');
                res.send('Transaction Decllined');
            }
        })
        // ask().then((perm)=>{
        //     if(perm === 'y'){
        //         if(req.param('accountToTransfer')){
        //             let t = sendTx.sendTransaction(req.param('accountToTransfer'));
        //             res.send("Transaction Signed Successfully");
        //             console.log('Transaction Signed Successfully');
        //         }else{
        //             res.send("Something went wrong!")
        //         }
        //     }else{
        //         console.log('Transaction Declined');
        //         res.send('Transaction Decllined');
        //     }
        // });
    }catch(e){
        console.log('Keys are not Set');
        res.send('Keys are not Set');
        console.log(e);
    }
    
});

app.get('/tx/contractFunction', function(req, res) {
    try{
        sendTxContract.setKeys(privKeyData[keyIndex].addr,privKeyData[keyIndex].privkey);
        ask().then((perm)=>{
            if(perm === 'y'){
                if(req.param('abi')|| req.param('contractAddress')){
                    // let t = sendTx.sendTransaction(req.param('accountToTransfer'));
                    // res.send("Transaction Signed Successfully");
                    // console.log('Transaction Signed Successfully');
                    let funcParam = {
                        abi: req.param('abi'),
                        contractAddress: req.param('contractAddress'),
                        encodedCall:  "instance.methods."+req.param('contractFunction')+".encodeABI()"
                    }
                    
                    let t = sendTxContract.doInteractionWithSC(funcParam);
                    res.send("Transaction Signed Successfully");
                    console.log("Transaction Signed Succssfully");
                }else{
                    res.send("Something went wrong!")
                }
            }else{
                console.log('Transaction Declined');
                res.send('Transaction Decllined');
            }
        });
    }catch(e){
        console.log('Keys are not Set');
        res.send('Keys are not Set');
        console.log(e);
    }
});

app.get('/tx/contractFunction/call', function(req, res) {
    
                if(req.param('abi')|| req.param('contractAddress')){
                    // let t = sendTx.sendTransaction(req.param('accountToTransfer'));
                    // res.send("Transaction Signed Successfully");
                    // console.log('Transaction Signed Successfully');
                    let funcParam = {
                        abi: req.param('abi'),
                        contractAddress: req.param('contractAddress'),
                        encodedCall:  "instance.methods."+req.param('contractFunction')
                    }
                    
                    let t = sendTxContractCall.callForValue(funcParam,(err,result)=>{
                        res.send(result);
                    });
                    
                }else{
                    res.send("Something went wrong!")
                }
});

function ask() {
    return new Promise((resolve, reject) => {
        rl.question('Sign Transaction (y/n): ', (input) => {
            resolve(input);
        });
    });
}
  // start the server
app.listen(port);
console.log('Panacea Service Working Fine :)');
function openWindow(){
    childWindow = new BrowserWindow({
        width: 800,
        height: 600,
        modal: true,
        show: false
     });
     childWindow.loadFile(`app/popup.html`);
     childWindow.once('ready-to-show', () => {
         childWindow.show();
     })
}