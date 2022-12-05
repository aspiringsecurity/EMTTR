const fs = require('fs');
const remote = require('electron').remote;

function sendIden(){
    fs.writeFileSync('app/idendata.txt', "yes");
    var window = remote.getCurrentWindow();
    window.close();
}

function DNTsendIden(){
    fs.writeFileSync('app/idendata.txt', "no");
    var window = remote.getCurrentWindow();
    window.close();
}


