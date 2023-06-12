import React from 'react'
import { Route, Switch, Redirect } from 'react-router-dom'
import BaseStyles from 'components/baseStyles'
import Home from 'screens/home'
import Mnemonic from 'screens/mnemonic'
import LoadMnemonic from 'screens/loadMnemonic'
import Encryption from 'screens/encryption'
import ViewNotes from 'screens/viewNotes'
import CreateNote from 'screens/createNote'
import {
  HOME,
  MNEMONIC,
  ENCRYPTION,
  LOAD_MNEMONIC,
  CREATE_NOTE,
  VIEW_NOTES
} from 'routes'
import AuthRoute from 'containers/authRoute'

const App = () => (
  <>
    <BaseStyles />
    <Switch>
      <Route exact path={HOME} component={Home} />
      <Route exact path={MNEMONIC} component={Mnemonic} />
      <Route exact path={LOAD_MNEMONIC} component={LoadMnemonic} />
      <AuthRoute exact path={ENCRYPTION} component={Encryption} />
      <AuthRoute exact path={CREATE_NOTE} component={CreateNote} />
      <AuthRoute exact path={VIEW_NOTES} component={ViewNotes} />

      <Redirect to="/" />
    </Switch>
  </>
)

export default App
