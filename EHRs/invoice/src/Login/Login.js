import React, { Component } from 'react';
import './Login.css';
import { Cloud } from '../storage/CloudStorage.js';
import { APP_NAME } from '../app-data.js';

class Login extends Component {

	constructor(props) {
      	super(props);
		
      	this.state = {
         	email: '', password: ''
      	};

      	this.cloud = new Cloud();	

   };

   updateEmail(e) {
      	this.setState({email: e.target.value});
   }

   updatePassword(e) {
      	this.setState({password: e.target.value});
   }

   doAuth(){
   		console.log("Loggin in... "+this.state.email);
   		const data = {email: this.state.email, password: this.state.password, appname: APP_NAME };
   		this.cloud._auth(data);
   }

	render(){
		return(
			<div className="Login-modal">
				<input type="text" id="email" value={ this.state.email } onChange={ this.updateEmail.bind(this) } placeholder="Email.."/>
				<input type="password" id="password" value={ this.state.password } onChange={ this.updatePassword.bind(this) } placeholder="Password.."/>
				<button onClick={ ()=> this.doAuth() }> Login </button>
			</div>
		);
	}
}

export default Login;