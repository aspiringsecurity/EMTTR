
const fs = require('fs');
const readline = require('readline');

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });


let getPrivateKeys = () => {
    try{
        let privateKeys = fs.readFileSync('keysdata.txt');
        return privateKeys;
    }catch(e){
        let keyPair = {
            privkey:"",
            addr: ""
        }
        let keys = [];
        
        rl.question('Please Enter the Private Key: ', (answer) => {
            keyPair.privkey = answer.toString();
            rl.question('Please Enter Address Associated with it: ',(answer)=>{
                keyPair.addr = answer.toString();
                rl.close();
                keys.push(keyPair);
                fs.writeFileSync('keysdata.txt',JSON.stringify(keys));
            });
        });
       
        return keys;
    }
}

let privKeyData = () => {
    let keys = JSON.parse(getPrivateKeys());
    return keys;   
};
module.exports = {
    privKeyData
}
