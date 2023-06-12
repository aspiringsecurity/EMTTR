import { container } from 'baseStyles'

const styles = theme => ({
  button: {
    marginTop: `${theme.spacing.unit}px`
  },
  container: {
    ...container,
    display: 'flex',
    alignItems: 'center'
  }
})

export default styles
