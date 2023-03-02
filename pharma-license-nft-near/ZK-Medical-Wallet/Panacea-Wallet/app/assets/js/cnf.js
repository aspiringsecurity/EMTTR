const electron = require('electron')
const path = require('path')
const { remote } = require('electron');
const BrowserWindow = electron.remote.BrowserWindow
const ipc = electron.ipcRenderer

function acceptRes(){
    ipc.send('ans', 'accept');
    var window = remote.getCurrentWindow();
    window.close();
}

function rejectRes(){
    ipc.send('ans', 'reject');
    var window = remote.getCurrentWindow();
    window.close();
}