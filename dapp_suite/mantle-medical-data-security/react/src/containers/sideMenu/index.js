import React from 'react'
import PropTypes from 'prop-types'
import SideMenu from 'components/sideMenu'
import Navlink from 'components/sideMenu/NavLink'
import ViewList from '@material-ui/icons/ViewList'
import Create from '@material-ui/icons/Create'
import EnhancedEncryption from '@material-ui/icons/EnhancedEncryption'
import PowerSettingsNew from '@material-ui/icons/PowerSettingsNew'
import { withStyles } from '@material-ui/core/styles'
import styles from './styles'
import {
  ENCRYPTION,
  VIEW_NOTES,
  CREATE_NOTE,
  HOME
} from 'routes'

const SideMenuContainer = ({ classes }) => {
  return (
    <SideMenu>
      <Navlink icon={ViewList} tooltip="View notes" to={VIEW_NOTES} />
      <Navlink icon={Create} tooltip="Create note" to={CREATE_NOTE} />
      <Navlink icon={EnhancedEncryption} tooltip="Encryption" to={ENCRYPTION} />
      {/* @TODO: Move logout button to bottom of nav bar */}
      <Navlink icon={PowerSettingsNew} tooltip="Logout" to={HOME} customStyles={classes.logout} />
    </SideMenu>
  )
}

SideMenuContainer.propTypes = {
  classes: PropTypes.object.isRequired
}

export default withStyles(styles)(SideMenuContainer)
