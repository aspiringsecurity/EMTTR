import React from 'react'
import PropTypes from 'prop-types'
import Text from '@material-ui/core/Typography'
import LinkButton from 'components/linkButton'
import { withStyles } from '@material-ui/core/styles'
import Grid from '@material-ui/core/Grid'
import { VIEW_NOTES } from 'routes'
import styles from './styles'

const Mnemonic = ({ mnemonic, classes }) => {
  return (
    <div className={classes.container}>
      <Grid container direction="column" alignItems="center">
        <Grid item>
          <Text>{mnemonic}</Text>
        </Grid>
        <Grid item>
          <LinkButton
            className={classes.button}
            color="primary"
            variant="contained"
            to={VIEW_NOTES}>
              Start demo
          </LinkButton>
        </Grid>
      </Grid>
    </div>
  )
}

Mnemonic.propTypes = {
  mnemonic: PropTypes.string.isRequired,
  classes: PropTypes.object.isRequired
}

export default withStyles(styles)(Mnemonic)
