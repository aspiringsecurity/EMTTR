import React from 'react'
import PropTypes from 'prop-types'
import { persistStore } from 'redux-persist'
import { PersistGate } from 'redux-persist/integration/react'
import { Provider as StoreProvider } from 'react-redux'
import { BrowserRouter as RouterProvider } from 'react-router-dom'
import { MuiThemeProvider } from '@material-ui/core/styles'
import * as theme from 'theme'
import { store } from 'model/store'
import { SnackbarProvider } from 'notistack'

const AppProvider = ({ children, theme, store }) => {
  const persistor = persistStore(store)
  const snackbarAnchor = { vertical: 'bottom', horizontal: 'left' }

  return (
    <StoreProvider store={store}>
      <PersistGate loading={null} persistor={persistor}>
        <RouterProvider>
          <MuiThemeProvider theme={theme}>
            <SnackbarProvider anchorOrigin={snackbarAnchor}>
              {children}
            </SnackbarProvider>
          </MuiThemeProvider>
        </RouterProvider>
      </PersistGate>
    </StoreProvider>
  )
}

AppProvider.defaultProps = {
  theme: theme.dark,
  store: store
}

AppProvider.propTypes = {
  children: PropTypes.node.isRequired,
  theme: PropTypes.object,
  store: PropTypes.object
}

export default AppProvider
