var path = require('path');

function login(){
    if(document.getElementById('password').value == "saru"){
        const remote = require('electron').remote;
  const BrowserWindow = remote.BrowserWindow;
//   const win = new BrowserWindow({
//     height: 600,
//     width: 800
//   });
  const path = require('path');

  let reqPath = path.join(__dirname, '../index.html');//It goes three folders or directories back from given __dirname.
  remote.getCurrentWindow().loadFile(reqPath);

//   win.loadURL(reqPath);
//     win.on('ready-to-show', () => {
//         win = remote.getCurrentWindow();
//         win.close();
//     })
    }else{
        alert('Login Failed');
    }
}

function create(){
   document.getElementById('c').insertAdjacentHTML('afterend', `<br><h4>Seed Phrase</h4><div class="txt1">alarm once odor swing strong final zone <br>ensure harvest gift venture tobacco</div><button>Save</button>`)
}