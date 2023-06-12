import { SIDE_MENU_WIDTH } from 'components/sideMenu'

const styles = theme => {
  const { spacing: { unit } } = theme

  return {
    button: {
      margin: `${unit}px`
    },
    privateKey: {
      color: '#9c2959'
    },
    container: {
      marginLeft: `${SIDE_MENU_WIDTH}px`,
      padding: `${unit * 4}px`
    },
    encrypted: {
      textOverflow: 'ellipsis',
      overflow: 'hidden'
    },
    margin: {
      marginBottom: `${unit * 2}px`
    },
    paper: {
      padding: `${unit * 2}px`,
      wordBreak: 'break-word'
    },
    heading: {
      marginBottom: `${unit * 2}px`
    },
    alignRight: {
      textAlign: 'right'
    }
  }
}

export default styles
