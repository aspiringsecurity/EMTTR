import React from 'react'
import { withRouter } from 'react-router-dom'
import Drawer from '@material-ui/core/Drawer'
import NavList from './NavList'

export const SIDE_MENU_WIDTH = 48

const drawerProps = {
  variant: 'permanent',
  anchor: 'left',
  style: {
    flexShrink: 0,
    width: SIDE_MENU_WIDTH
  },
  PaperProps: {
    style: {
      width: SIDE_MENU_WIDTH
    }
  }
}

const SideMenu = ({ children }) => (
  <Drawer {...drawerProps}>
    <NavList>{children}</NavList>
  </Drawer>
)

export default withRouter(SideMenu)
