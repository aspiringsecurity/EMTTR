import React, { Component } from 'react';
import './Files.css';
import * as AppGeneral from '../socialcalc/AppGeneral';
import { Local } from '../storage/LocalStorage.js';
import { DATA } from '../app-data.js';

class Files extends Component {

	constructor(props){
		super(props);
		this.store = new Local(this.props.file);	
		this.state = {
			files: this.store._getAllFiles()
		}
	}

	editFile(key){
		const data = this.store._getFile(key);
		// console.log(JSON.stringify(data));
		AppGeneral.viewFile(key, decodeURIComponent(data.content));
		this.props.updateSelectedFile(key);
	}

	deleteFile(key){
		event.preventDefault()
    	const result = window.confirm(`Do you want to delete the ${key} file?`)
    	if (result) {
    	  // Delete file
    	  this.store._deleteFile(key);
    	  this.setState({ files: this.store._getAllFiles() });
    	  this.loadDefault();
    	}
	}

	loadDefault(){
		const msc = DATA['home'][AppGeneral.getDeviceType()]['msc'];
		AppGeneral.viewFile('default', JSON.stringify(msc));
		this.props.updateSelectedFile('default');
	}

	render() {
		const files = this.store._getAllFiles();
		// console.log(JSON.stringify(files));
		let fileList = Object.keys(files).map(key => {
			// console.log(key);
			return <div key={key}><li>{ key } <span>{ this._formatDate(files[key]) }</span></li>
					<button onClick={()=>{ this.editFile(key)} }>Edit</button>
					<button onClick={()=>{ this.deleteFile(key)} }>Delete</button>
					</div>;
		});

		return (
			<div className="file">
				<ul>
					{ fileList }
				</ul>
			</div>
		);
	}

	_formatDate(date){
		return new Date(date).toLocaleString();
	}

}

export default Files;