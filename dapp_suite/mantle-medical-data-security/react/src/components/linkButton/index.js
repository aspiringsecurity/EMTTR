import { Link } from 'react-router-dom'
import Button from '@material-ui/core/Button'
import { withProps } from 'recompose'

const LinkButton = withProps({ component: Link })(Button)

export default LinkButton
