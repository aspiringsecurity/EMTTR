import { Switch, Route } from 'react-router-dom';
import PropsRoute from './PropsRoute';
import Home from './Home';
import UserTweets from './UserTweets';
import CreateUser from './CreateUser';
import UpdateUser from './UpdateUser';
import Error from './Error';
import React, { Component } from 'react';

/**
 * Class representing the area below the header.
 * The component rendering in this area is controlled by
 * a @external "BrowserRouter"
 * 
 * @extends React.Component
 */
class Main extends Component {

  //#region Constructor
  constructor(props){
    super(props);
  }
  //#endregion

  //#region React lifecycle events
  render () {
    return (
      <main>
        <Switch>
          <Route exact path='/' component={Home}/>
          <PropsRoute path='/@:username' component={UserTweets} {...this.props}/>
          <PropsRoute path='/create' component={CreateUser} {...this.props}/>
          <PropsRoute path='/update/@:username' component={UpdateUser} {...this.props}/>
          <PropsRoute path='/whoopsie' component={Error} {...this.props}/>
        </Switch>
      </main>
    )
  }
  //#endregion
}

export default Main
