import React, { Component } from 'react'
import PropTypes from 'prop-types'
import Text from '@material-ui/core/Typography'
import Typography from '@material-ui/core/Typography'
import { withStyles } from '@material-ui/core/styles'
import Grid from '@material-ui/core/Grid'
import TextField from '@material-ui/core/TextField'
import styles from './styles'
import Mantle from '@appliedblockchain/mantle-core'
import Button from '@material-ui/core/Button'
import { withSnackbar } from 'notistack'
import { compose } from 'redux'
import Paper from '@material-ui/core/Paper'
import VpnKey from '@material-ui/icons/VpnKey'
import IconButton from '@material-ui/core/IconButton'
import Tooltip from '@material-ui/core/Tooltip'
import SideMenu from 'containers/sideMenu'

class Encryption extends Component {
  constructor(props) {
    super(props)

    this.mantle = new Mantle()
    this.mantle.loadMnemonic(props.mnemonic)
    this.copyPublicKey = this.copyToClipboard.bind(this, this.mantle.getPublicKey('hex'))
    this.copyPrivateKey = this.copyToClipboard.bind(this, this.mantle.getPrivateKey('hex'))

    this.state = {
      message: '',
      encrypted: '',
      decrypted: '',
      publicKey: '',
      privateKey: ''
    }
  }

  handleChange = key => event => {
    this.setState({
      [key]: event.target.value
    })
  }

  encrypt = () => {
    const { enqueueSnackbar } = this.props
    try {
      const { message } = this.state
      const encrypted = Mantle.encrypt(message, this.state.publicKey)
      this.setState({ encrypted })
    } catch (err) {
      enqueueSnackbar('Encryption failed', { variant: 'error' })
    }
  }

  decrypt = () => {
    const { enqueueSnackbar } = this.props
    try {
      const { encrypted, privateKey } = this.state
      const decrypted = Mantle.decrypt(encrypted, privateKey)
      this.setState({ decrypted })
      enqueueSnackbar('decryption successful', { variant: 'success' })
    } catch (err) {
      enqueueSnackbar('Decryption failed', { variant: 'error' })
      this.setState({ decrypted: '' })
    }
  }

  copyToClipboard = text => {
    const input = document.createElement('input')
    input.setAttribute('value', text)

    document.body.appendChild(input)
    input.select()

    document.execCommand('copy')
    document.body.removeChild(input)
  }

  render() {
    const { classes } = this.props

    return (
      <div className={classes.container}>
        <SideMenu />
        <Grid container justify="center" spacing={24}>
          <Grid item xs={10}>
            <Typography variant="h4">Asymmetric Encryption Demo</Typography>
          </Grid>
          <Grid item xs={10}>
            <Paper className={classes.paper}>
              <Text>Account details</Text>
              <Typography variant="caption">Click the key icons to copy your public or private key to your clipboard</Typography>
              <Tooltip title="Public key">
                <IconButton onClick={this.copyPublicKey}>
                  <VpnKey />
                </IconButton>
              </Tooltip>

              <Tooltip title="Private key" className={classes.privateKey}>
                <IconButton onClick={this.copyPrivateKey}>
                  <VpnKey />
                </IconButton>
              </Tooltip>
            </Paper>
          </Grid>

          <Grid item xs={10}>
            <Paper className={classes.paper}>
              <Text className={classes.heading}>Encrypt a message with your public key</Text>
              <Grid container spacing={16}>
                <Grid item xs={3}>
                  <TextField
                    label="Message"
                    variant="outlined"
                    value={this.state.message}
                    onChange={this.handleChange('message')}
                  />
                </Grid>
                <Grid item xs={3}>
                  <TextField
                    label="Encryption key"
                    variant="outlined"
                    value={this.state.publicKey}
                    onChange={this.handleChange('publicKey')}
                  />
                </Grid>
                <Grid item xs={3}></Grid>
                <Grid item xs={3} className={classes.alignRight}>
                  <Button
                    color="primary"
                    variant="contained"
                    className={classes.button}
                    onClick={this.encrypt}>
                      Encrypt
                  </Button>
                </Grid>
              </Grid>
            </Paper>
          </Grid>

          <Grid item xs={10}>
            <Paper className={classes.paper}>
              <Text className={classes.heading}>Decrypt your encrypted message with your private key</Text>
              <Grid container spacing={16}>
                <Grid item xs={3}>
                  <TextField
                    label="Private key"
                    variant="outlined"
                    value={this.state.privateKey}
                    onChange={this.handleChange('privateKey')}
                  />
                </Grid>

                <Grid item xs={6}></Grid>

                <Grid item xs={3} className={classes.alignRight}>
                  <Button
                    color="primary"
                    variant="contained"
                    className={classes.button}
                    onClick={this.decrypt}>
                      Decrypt
                  </Button>
                </Grid>
              </Grid>
            </Paper>
          </Grid>

          <Grid item xs={10}>
            <Paper className={classes.paper}>
              <Text className={classes.heading}>Results</Text>

              <div className={classes.margin}>
                <Text color="primary">Encrypted message</Text>
                <Text className={classes.encrypted}>{this.state.encrypted || 'PENDING'}</Text>
              </div>

              <Text color="primary">Decrypted message</Text>
              <Text>{this.state.decrypted || 'PENDING'}</Text>
            </Paper>
          </Grid>
        </Grid>
      </div>
    )
  }
}

Encryption.propTypes = {
  classes: PropTypes.object.isRequired,
  enqueueSnackbar: PropTypes.func.isRequired,
  mnemonic: PropTypes.string.isRequired
}

export default compose(
  withSnackbar,
  withStyles(styles)
)(Encryption)
