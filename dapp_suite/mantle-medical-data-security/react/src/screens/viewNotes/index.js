import ViewNotes from './ViewNotes'
import { connect } from 'react-redux'
import { fetchNotes } from 'model/notes'

const mapState = ({ notes }) => ({
  notes
})

export default connect(mapState, {
  fetchNotes
})(ViewNotes)
