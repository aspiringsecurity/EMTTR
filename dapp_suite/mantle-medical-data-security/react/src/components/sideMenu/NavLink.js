import React from 'react'
import PropTypes from 'prop-types'
import { withStyles } from '@material-ui/core/styles'
import { withRouter, Link } from 'react-router-dom'
import { compose } from 'recompose'
import Tooltip from '@material-ui/core/Tooltip'
import IconButton from '@material-ui/core/IconButton'
import styles from './styles'

const NavLink = ({ to, location, classes, customStyles, tooltip, icon: Icon }) => {
  const rootProps = {
    to,
    component: Link
  }

  return (
    <Tooltip title={tooltip} placement="right" className={`${classes.container} ${customStyles}`} {...rootProps}>
      <IconButton>
        <Icon className={location.pathname === to ? classes.selected : null} />
      </IconButton>
    </Tooltip>
  )
}

NavLink.propTypes = {
  to: PropTypes.string.isRequired,
  location: PropTypes.object.isRequired
}

export default compose(
  withRouter,
  withStyles(styles)
)(NavLink)
