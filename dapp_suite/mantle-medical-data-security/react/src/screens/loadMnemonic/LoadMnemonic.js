import React, { Component } from 'react'
import PropTypes from 'prop-types'
import Button from '@material-ui/core/Button'
import { withStyles } from '@material-ui/core/styles'
import Grid from '@material-ui/core/Grid'
import TextField from '@material-ui/core/TextField'
import Mantle from '@appliedblockchain/mantle-core'
import { VIEW_NOTES } from 'routes'
import { withSnackbar } from 'notistack'
import styles from './styles'
import { compose } from 'redux'

class LoadMnemonic extends Component {
  constructor(props) {
    super(props)
    this.handleLoadMnemonic = this.handleLoadMnemonic.bind(this)
    this.state = {
      mnemonic: ''
    }
  }

  handleChange = key => event => {
    this.setState({
      [key]: event.target.value
    })
  }

  handleLoadMnemonic = () => {
    const { mnemonic } = this.state
    const { enqueueSnackbar } = this.props

    try {
      const mantle = new Mantle()
      mantle.loadMnemonic(mnemonic || 'invalid') // Validate the mnemonic

      const { history, loadMnemonic } = this.props
      loadMnemonic(mnemonic)
      history.push(VIEW_NOTES)
    } catch (err) {
      enqueueSnackbar('Failed to load mnemonic: please ensure you are entering a valid 12 word mnemonic', { variant: 'error' })
    }
  }

  render() {
    const { classes } = this.props

    return (
      <div className={classes.container}>
        <Grid container direction="column" alignItems="center">
          <Grid item>
            <TextField
              label="Mnemonic"
              variant="outlined"
              value={this.state.mnemonic}
              onChange={this.handleChange('mnemonic')}
            />
          </Grid>
          <Grid item>
            <Button
              className={classes.button}
              color="primary"
              variant="contained"
              onClick={this.handleLoadMnemonic}>
              Start demo
            </Button>
          </Grid>
        </Grid>
      </div>
    )
  }
}

LoadMnemonic.propTypes = {
  classes: PropTypes.object.isRequired,
  enqueueSnackbar: PropTypes.func.isRequired,
  history: PropTypes.object.isRequired,
  loadMnemonic: PropTypes.func.isRequired
}

export default compose(
  withSnackbar,
  withStyles(styles)
)(LoadMnemonic)
