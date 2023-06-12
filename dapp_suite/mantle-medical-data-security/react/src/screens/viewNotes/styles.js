import { SIDE_MENU_WIDTH } from 'components/sideMenu'

const styles = theme => {
  const { spacing: { unit } } = theme

  return {
    container: {
      marginLeft: `${SIDE_MENU_WIDTH}px`,
      padding: `${unit * 4}px`
    },
    paper: {
      padding: `${unit * 2}px`,
      wordBreak: 'break-word'
    },
    alignRight: {
      textAlign: 'right'
    },
    restricted: {
      opacity: 0.5
    }
  }
}

export default styles
