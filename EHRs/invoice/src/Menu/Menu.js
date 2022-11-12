import React, { Component } from 'react';
import './Menu.css';
import * as AppGeneral from '../socialcalc/AppGeneral';
import { File, Local } from '../storage/LocalStorage.js';
import { DATA } from '../app-data.js';
import Login from '../Login/Login';
import AWS from 'aws-sdk';
//import config from '../config';


const ses = new AWS.SES({
	  apiVersion: '2010-12-01',
	  accessKeyId: 'AKIAIUAT6PWCBX54OQEA',
	  "secretAccessKey": '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',
	  "region": "us-west-1",
	});


class Menu extends Component {

	constructor(props){
		super(props);
		this.store = new Local(this.props.file);
		this.state = { login : false , openLogin: false };	
	}

	doPrint(){
		const content = AppGeneral.getCurrentHTMLContent();
		var printWindow = window.open('','','left=100,top=100');
		printWindow.document.write(content);
		printWindow.print();
		printWindow.close();
	}

	doSave(){
		if(this.props.file === "default"){
			window.alert(`Cannot update ${this.props.file} file! `);
			return;
		}
		const content = encodeURIComponent(AppGeneral.getSpreadsheetContent());
		const data = this.store._getFile(this.props.file);
		const file = new File(data.created, new Date().toString(), content, this.props.file);
		this.store._saveFile(file);
		this.props.updateSelectedFile(this.props.file);
		window.alert(`File ${this.props.file} updated successfully! `);
	}

	doSaveAs(){
		event.preventDefault();
		const filename = window.prompt("Enter filename : ");
	    if(filename) {
			if(this._validateName(filename)){
				// filename valid . go on save
				const content = encodeURIComponent(AppGeneral.getSpreadsheetContent());
				// console.log(content);
				const file = new File(new Date().toString(), new Date().toString(), content, filename);
				// const data = { created: file.created, modified: file.modified, content: file.content, password: file.password };
				// console.log(JSON.stringify(data));
				this.store._saveFile(file);
				this.props.updateSelectedFile(filename);
				window.alert(`File ${filename} saved successfully! `);
			}
			else{
				window.alert(`Filename cannot be ${this.props.file}`);
			}
		}
		
	}

	newFile(){
		if(this.props.file !== 'default'){
			const content = encodeURIComponent(AppGeneral.getSpreadsheetContent());
			const data = this.store._getFile(this.props.file);
			const file = new File(data.created, new Date().toString(), content, this.props.file);
			this.store._saveFile(file);
			this.props.updateSelectedFile(this.props.file);
		}
		const msc = DATA['home'][AppGeneral.getDeviceType()]['msc'];
		AppGeneral.viewFile('default', JSON.stringify(msc));
		this.props.updateSelectedFile('default');
	}

	auth(loggedIn){
		if(loggedIn){
			// console.log("log out..");
			this.setState({login: false , openLogin: false});
		}
		else{
			// console.log("log in..");
			this.setState({login: true , openLogin: true });
		}
	}

	sendEmail(){
		// Prepare values to send with email
      const emailParams = {
        Destination: { ToAddresses: [ '<aspiringuserapps@gmail.com>' ] },
        Message: {
          Body: { Text: {
            Data: 'This is a test email' } },
          Subject: { Data: 'Contact Form' }
        },
        ReplyToAddresses: [''],
        Source: '<aspiring.investments@gmail.com>', // this has to be verified email in SES
      };

      ses.sendEmail(emailParams, function(error, data) {
        if (error) {
           // handle error
        } else {
           // handle success
           alert("Done");
        }
      });
	}

	render() {
		return (
			<div className="Menu">
				<button onClick={() => this.doSave()} > Save  </button>
				<button onClick={() => this.doSaveAs()} > Save As </button>
				<button onClick={() => this.doPrint()} > Print </button>
				<button onClick={() => this.sendEmail()} > Email </button>
				<button onClick={() => this.newFile()} > New File </button>
				<button onClick={() => this.auth(this.state.login)} > {this.state.login ? `Logout` : `Login` } </button>
				{ this.state.openLogin ? <Login /> : null  }
			</div>
		);
	}

	/* Utility functions */
	_validateName(filename){

		filename = filename.trim();
		if(filename === "default" || filename === "Untitled"){
			// return 'Cannot update default file!';
			return false;
		}
		else if(filename === '' || !filename){
			// this.showToast('Filename cannot be empty');
			return false;
		}
		else if(filename.length > 30) {
			// this.showToast('Filename too long');
			return false;
		}
		else if(/^[a-zA-Z0-9- ]*$/.test(filename) === false) {
			// this.showToast('Special Characters cannot be used');
			return false;
		}
		return true;
	}

	_formatString(filename){
		/* Remove whitespaces */
		while(filename.indexOf(" ") !== -1){
			filename = filename.replace(" ","");
		}
		return filename;
	}

}

export default Menu;