import { createMuiTheme } from '@material-ui/core/styles'

const primary = {
  light: '#d8cfb3',
  main: '#9c2959',
  dark: '#861b47'
}

const light = createMuiTheme({
  typography: { useNextVariants: true },
  palette: { primary }
})

const dark = createMuiTheme({
  typography: {
    useNextVariants: true,
    fontFamily: 'Montserrat, sans-serif'
  },
  palette: {
    type: 'dark',
    primary,
    divider: '#232a38',
    background: {
      default: '#0f1723',
      paper: '#1a222f'
    }
  }
})

export { light, dark }
