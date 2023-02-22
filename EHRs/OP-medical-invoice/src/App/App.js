import React, { Component } from 'react';
import './App.css';
import * as AppGeneral from '../socialcalc/AppGeneral';
import { DATA } from '../app-data.js';

import Menu from '../Menu/Menu';
import Files from '../Files/Files';

class App extends Component {

  constructor(props) {
    super(props);
    this.state = {selectedFile: "default" , device: AppGeneral.getDeviceType() , listFiles: false };
    this.updateSelectedFile = this.updateSelectedFile.bind(this);
    this.toggleListFiles = this.toggleListFiles.bind(this);
  }

  updateSelectedFile(selectedFile){
    this.setState({
      selectedFile: selectedFile
    });
  }

  toggleListFiles(){
    this.setState(prevState => ({
      listFiles: !prevState.listFiles
    }));
  }

  componentDidMount(){
    let data = DATA['home'][this.state.device]['msc'];
    AppGeneral.initializeApp(JSON.stringify(data));
  }

  activateFooter(footer){
    // console.log("Button pressed! "+footer);
    AppGeneral.activateFooterButton(footer);
  }

  render() {
    let footers = DATA['home'][this.state.device]['footers'];
    
    let footersList = footers.map((footerArray, i) => {
        // console.log(footerArray.name);
        // console.log(footerArray.index);
        return <button className="button button-outline" key={footerArray.index} onClick={() => this.activateFooter(footerArray.index)}> {footerArray.name} </button>
        
    });

    return (
      <div className="App">
        <div className="App-header"> 
          <span>Editing: { this.state.selectedFile } </span>
          <button className="App-list" onClick={ this.toggleListFiles } >List Files  </button> 
          
        </div>
        <div className="App-menu"> <Menu file={this.state.selectedFile} updateSelectedFile={this.updateSelectedFile} /> </div>
        <ul className="App-footers"> { footersList } </ul>
        <div id="workbookControl"></div>
        <div id="tableeditor">editor goes here</div> 
        <div id="msg"></div> 
        {this.state.listFiles ? <div className="App-files"> 
                                <Files file={this.state.selectedFile} updateSelectedFile={this.updateSelectedFile}/> </div> : null} 
      </div>
    );
  }
}

export default App;
