//Electron Modules
const electron = require('electron')
const path = require('path')
const BrowserWindow = electron.remote.BrowserWindow
const ipc = electron.ipcRenderer

// Modules
const express = require('express');
let fs = require('fs');
const crypto = require('crypto');

// Custom Modules

const app = express();

const port = process.env.PORT || 8080;

let accountToTransfer = "";

var account = "0x90f8bf6a479f320ead074411a4b0e7944ea8c9c1";
var privateKey = "4f3edf983ac636a65a842ce7c78d9aa706d3b113bce9c46f30d7d21715b23b1d";


let tranHash;
app.use(function(req, res, next) {
    res.setHeader("Access-Control-Allow-Origin", "*");
    // Request headers you wish to allow
    res.setHeader("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
    // Set to true if you need the website to include cookies in the requests sent
    res.setHeader('Access-Control-Allow-Credentials', true);
    // Pass to next layer of middleware
    next();
});

setKeys(account,privateKey);

app.get('/testWallet', function(req, res) {
    res.send('Panacea v1.0');
});
app.get('/bal', (req,res) => {
    res.send(web3.eth.getBalance(account));
})
app.get('/acct', (req,res) => {
    res.send(account);
})
app.get('/net', (req,res) => {
    let path = require('path');
    let netFile = path.join(__dirname, 'assets/js/network.txt')
    console.log(fs.readFileSync(netFile).toString());
    res.send(fs.readFileSync(netFile).toString());
    
})

app.get('/tx/sendTransaction',(req,res)=>{
     accountToTransfer = req.param('accountToTransfer');
     amountToTransfer = req.param('amount')
    // createPermWindow(accountToTransfer,(resAns)=>{
    //     res.send(resAns);
    // });
    // createPermWindow(accountToTransfer,(r)=>{
    //     res.end(r);
    // })
    openWindow();
        childWindow.on('closed', () => {
            win = null
            let s = fs.readFileSync('app/resp.txt');
            console.log(s.toString());
            if(s == 'yes'){
                let t = sendTransaction(accountToTransfer, amountToTransfer, (tH)=>{
                    tranHash = tH;
                    console.log('Transaction Signed Successfully');
                    console.log(tranHash);
                    accountToTransfer = "";
                    //callback(tH);
                    res.send(tranHash);
                });
                }else{
                    res.send("Transaction Declined")
                }
            // else{
            //     console.log('Transaction Declined');
            //     res.send('Transaction Decllined');
            // }
        })
    // if(confirm('Sign the Transaction: ')){
    //     let t = sendTransaction(accountToTransfer, amountToTransfer,(tH)=>{
    //         tranHash = tH;
    //         console.log('Transaction Signed Successfully');
    //         console.log(tranHash);
    //         accountToTransfer = "";
    //         //callback(tH);
    //         res.send(tranHash);
    //     });
    // }else{
    //     res.send('Transaction Declined');
    // }

});

let createPermWindow = (accountToTransfer,callback) => {
    const modalPath = path.join('file://', __dirname, 'cnf.html')
    let win = new BrowserWindow({ 
        frame: false, 
        alwaysOnTop: true,    // Add this line
        width: 200, 
        height: 200
     })
    win.on('close', function () { win = null })
    win.loadURL(modalPath)
    win.show()

    ipc.on('ans', function (event, arg) {
        if(arg  == 'accept'){
            // let t = sendTransaction(accountToTransfer,(tH)=>{
            //     tranHash = tH;
            //     console.log('Transaction Signed Successfully');
            //     console.log(tranHash);
            //     accountToTransfer = "";
            //     callback(tH);
            // });
            console.log(arg);
        }else{
            callback('Transaction Failed');
        }
    });
}

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
        // console.log(funcParam);
        let t = callForValue(funcParam,(err,result)=>{
            res.send(result);
        });
        
    }else{
        res.send("Something went wrong!")
    }
});

app.get('/tx/contractFunction', function(req, res) {
    //if(req.param('abi')|| req.param('contractAddress')){
        let funcParam = {
            abi: req.param('abi'),
            contractAddress: req.param('contractAddress'),
            encodedCall:  "instance.methods."+req.param('contractFunction')+".encodeABI()"
        }
        /////////////////////////////////////////////////////////

        //fs.writeFileSync('app/trans.txt', );
        openWindow();
        childWindow.on('closed', () => {
            win = null
            let s = fs.readFileSync('app/resp.txt');
            console.log(s.toString());
            if(s == 'yes'){
                let t = doInteractionWithSC(funcParam,(tH)=>{
                    res.send(tH);
                });
                // res.send("Transaction Signed Successfully");
                console.log("Transaction Signed Succssfully");
                }else{
                    res.send("Transaction Declined")
                }
            // else{
            //     console.log('Transaction Declined');
            //     res.send('Transaction Decllined');
            // }
        })

        /////////////////////////////////////////////////////////





    //     if(confirm('Sign The Transaction?')){
    //     let t = doInteractionWithSC(funcParam,(tH)=>{
    //         res.send(tH);
    //     });
    //     // res.send("Transaction Signed Successfully");
    //     console.log("Transaction Signed Succssfully");
    // }else{
    //     res.send('Transaction Declined');
    // }
    // }else{
    //     res.send("Something went wrong!")
    // }
});

app.get('/enc', function(req,res){
    let dataToEnc = req.param("enc");
    console.log('Starting the Encrypting Process...');
    res.send(dataToEnc);
})
app.get('/passGen', function(req,res) {
  openWindowPassword();  
  childWindow.on('closed',  () => {
    win = null
    let s = fs.readFileSync('app/passGen.txt');
    console.log(s.toString());
    var xhttp = new XMLHttpRequest();
        xhttp.onreadystatechange = function() {
            if (this.readyState == 4 && this.status == 200) {
            // Typical action to be performed when the document is ready:
           res.send(xhttp.responseText.toString());
           fs.writeFileSync("app/hash.txt", xhttp.responseText);
            }
        };
        xhttp.open("GET", "http://192.168.10.130:3000/password/hash?password="+s.toString(), true);
        xhttp.send();
        })
})

app.get('/passAuth', function(req,res){
    openWindowPassword();
    childWindow.on('closed', () => {
        win = null
        let s = fs.readFileSync('app/passGen.txt');
        console.log(s.toString());
        // API To generate Proof
        let hashData = fs.readFileSync("app/hash.txt");
        console.log(JSON.parse(hashData).hash);
        var xhttp = new XMLHttpRequest();
        xhttp.onreadystatechange = function() {
            if (this.readyState == 4 && this.status == 200) {
            // Typical action to be performed when the document is ready:
           res.send(xhttp.responseText.toString());
        //    fs.writeFileSync("app/hash.txt", xhttp.responseText);
            }
        };
        xhttp.open("GET", "http://192.168.10.130:3000/password/proof?password="+s.toString()+"&passwordhash="+JSON.parse(hashData).hash, true);
        xhttp.send();
        })
})

app.get('/age/proof', (req,res) => {
    openWindowIdentity();
    childWindow.on('closed', () => {
        win = null
        let s = fs.readFileSync('app/idendata.txt');
        console.log(s.toString());
        // API To generate Proof
        if(s.toString() == "yes"){
            var xhttp = new XMLHttpRequest();
        xhttp.onreadystatechange = function() {
            if (this.readyState == 4 && this.status == 200) {
            // Typical action to be performed when the document is ready:
           res.send(xhttp.responseText.toString());
           fs.writeFileSync("app/idenproof.txt", xhttp.responseText);
            }
        };
        xhttp.open("GET", "http://192.168.10.130:3000/age/proof?age=21&agebar=18", true);
        xhttp.send();
        
        }else{
            res.send('User Denied');
        }
    })
})


app.listen(port,()=>{
    console.log(`Panacea Services Working Fine :)`);
})

function openWindow(){
    childWindow = new BrowserWindow({
        width: 600,
        height: 700,
        modal: true,
        show: false,
        frame: false
     });
     childWindow.loadFile(`app/popup.html`);
     childWindow.once('ready-to-show', () => {
         childWindow.show();
     })
}

function openWindowPassword(){
    childWindow = new BrowserWindow({
        width: 300,
        height: 400,
        modal: true,
        show: false,
        frame: false
    });
    childWindow.loadFile('app/passpopup.html');
    childWindow.once('ready-to-show', () => {
        childWindow.show();
    })
}

function openWindowIdentity(){
    childWindow = new BrowserWindow({
        width: 400,
        height: 500,
        modal: true,
        show: false,
        frame: false
    })
    childWindow.loadFile('app/idenpopup.html');
    childWindow.once('ready-to-show', () => {
        childWindow.show();
    })
}