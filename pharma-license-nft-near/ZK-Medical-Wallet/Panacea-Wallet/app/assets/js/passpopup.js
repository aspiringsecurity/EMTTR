const fs = require('fs');
const remote = require('electron').remote;

function genPass(){
    let data = document.getElementById('password').value;
    fs.writeFileSync('app/passGen.txt', data);
    var window = remote.getCurrentWindow();
    window.close();
}
