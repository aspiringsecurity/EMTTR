import React, { Component } from 'react'
import PropTypes from 'prop-types'
import Text from '@material-ui/core/Typography'
import { withStyles } from '@material-ui/core/styles'
import Grid from '@material-ui/core/Grid'
import styles from './styles'
import { withSnackbar } from 'notistack'
import { compose } from 'redux'
import Paper from '@material-ui/core/Paper'
import SideMenu from 'containers/sideMenu'
import Typography from '@material-ui/core/Typography'

class ViewNotes extends Component {
  constructor(props) {
    super(props)
    this.state = {
      notes: []
    }
  }

  componentDidMount() {
    const { fetchNotes } = this.props
    fetchNotes()
  }

  render() {
    const { classes, notes } = this.props

    return (
      <div className={classes.container}>
        <SideMenu />
        <Grid container justify="center" spacing={24}>
          <Grid item xs={10}>
            <Typography variant="h4">Notes</Typography>
          </Grid>
          { notes.length ? notes.map((note, idx) => (
            <Grid item xs={10} key={idx} className={note.viewable ? null : classes.restricted}>
              <Paper className={classes.paper}>
                <Text>Tag: {note.tag}</Text>
                { note.viewable
                  ? <Text>{note.decrypted}</Text>
                  : <Text>This note has not been shared with you</Text>
                }
              </Paper>
            </Grid>
          )) : (
            <Grid item xs={10}>
              <Paper className={classes.paper}>
                <Text>No notes available</Text>
              </Paper>
            </Grid>
          ) }
        </Grid>
      </div>
    )
  }
}

ViewNotes.propTypes = {
  fetchNotes: PropTypes.func.isRequired,
  classes: PropTypes.object.isRequired,
  notes: PropTypes.array
}

export default compose(
  withSnackbar,
  withStyles(styles)
)(ViewNotes)
