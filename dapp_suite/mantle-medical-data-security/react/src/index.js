import React from 'react'
import ReactDOM from 'react-dom'
import { document } from 'global'
import App from 'containers/app'
import AppProvider from 'containers/appProvider'

ReactDOM.render(
  <AppProvider>
    <App />
  </AppProvider>,
  document.getElementById('root')
)
