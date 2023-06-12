import React from 'react'
import PropTypes from 'prop-types'
import Main from 'components/main'
import LinkButton from 'components/linkButton'
import Grid from '@material-ui/core/Grid'
import Button from '@material-ui/core/Button'
import Mantle from '@appliedblockchain/mantle-core'
import { MNEMONIC, LOAD_MNEMONIC } from 'routes'

const Home = props => {
  const handleLoadMnemonic = async () => {
    const { history, loadMnemonic } = props
    const mnemonic = Mantle.generateMnemonic()
    await loadMnemonic(mnemonic, true)
    history.push(MNEMONIC)
  }

  return (
    <Main style={{ display: 'flex', alignItems: 'center' }}>
      <Grid container spacing={24}>
        <Grid item xs={12}>
          <Button
            fullWidth
            color="primary"
            variant="contained"
            onClick={handleLoadMnemonic}>
              Generate Mnemonic
          </Button>
        </Grid>
        <Grid item xs={12}>
          <LinkButton
            fullWidth
            color="primary"
            variant="contained"
            to={LOAD_MNEMONIC}>
              Load mnemonic
          </LinkButton>
        </Grid>
      </Grid>
    </Main>
  )
}

Home.propTypes = {
  history: PropTypes.object.isRequired,
  loadMnemonic: PropTypes.func.isRequired
}

export default Home
