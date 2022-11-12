const storage = window.localStorage;

export class File{
	
	created: string;
	modified: string;
	name: string
	content: string;
	password: string;

	constructor(created: string, modified: string, content: string, name: string, password ?: string ){
		this.created = created;
		this.modified = modified;
		this.content = content;
		this.name = name;
		this.password = password;
	}

}

export class Local {

	constructor(){
		this.storage = storage;
		this.token = null;
	}

	_saveFile(file: File){
		// console.log(file.password);
		let data = {created: file.created, modified: file.modified, content: file.content, password: file.password};
		this.storage.setItem(file.name, JSON.stringify(data));
	}

	_getFile(name){
		const rawData = this.storage.getItem(name);
    	return JSON.parse(rawData);
	}

	_getAllFiles(){
		let arr = {};
		for(let i = 0; i< window.localStorage.length; i++){
			var fname = window.localStorage.key(i);
    		// console.log(fname);
    		const data = this._getFile(fname);
    		arr[fname] = data.modified;
		}
		return arr;
	}

	_deleteFile(name){
		console.log("deleting file "+name);
		this.storage.removeItem(name);
	}

}