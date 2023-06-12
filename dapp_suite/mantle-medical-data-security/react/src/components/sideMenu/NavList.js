import { withProps } from 'recompose'
import List from '@material-ui/core/List'
import { withTheme } from '@material-ui/core/styles'

const NavList = withProps({
  component: 'nav'
})(List)

export default withTheme()(NavList)
