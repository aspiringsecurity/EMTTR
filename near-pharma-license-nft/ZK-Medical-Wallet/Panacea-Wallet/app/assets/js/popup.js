const fs = require('fs');
const remote = require('electron').remote;

function accept(){
    //let data = document.getElementById('t1').value + trans;
    fs.writeFileSync('app/resp.txt', "yes");
    var window = remote.getCurrentWindow();
    window.close();
}

function deny(){
    //let data = document.getElementById('t1').value + trans;
    fs.writeFileSync('app/resp.txt', "no");
    var window = remote.getCurrentWindow();
    window.close();
}