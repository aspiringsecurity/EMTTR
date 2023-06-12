import { SIDE_MENU_WIDTH } from 'components/sideMenu'
import { USER_MENU_WIDTH } from './CreateNote'

const styles = theme => {
  const { spacing: { unit } } = theme

  return {
    container: {
      marginLeft: `${SIDE_MENU_WIDTH}px`,
      marginRight: `${USER_MENU_WIDTH}px`,
      padding: `${unit * 4}px`
    },
    usersContainer: {
      padding: `${unit * 2}px`
    },
    paper: {
      padding: `${unit * 2}px`,
      wordBreak: 'break-word'
    },
    userPaper: {
      padding: `${unit * 2}px`,
      wordBreak: 'break-word',
      backgroundColor: '#1b2635'
    },
    marginBottom: {
      marginBottom: `${unit * 2}px`
    },
    alignRight: {
      textAlign: 'right'
    },
    userOptions: {
      display: 'flex',
      justifyContent: 'space-between',
      alignItems: 'center',
      marginBottom: `${unit * 2}px`,
      borderBottom: '1px solid #616871',
      paddingBottom: `${unit}px`
    },
    sharing: {
      color: '#6cc3ce'
    }
  }
}

export default styles
