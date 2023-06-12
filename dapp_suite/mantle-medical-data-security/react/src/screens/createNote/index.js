import CreateNote from './CreateNote'
import { connect } from 'react-redux'
import { createNote } from 'model/notes'
import { fetchUsers } from 'model/users'

const mapState = ({ users }) => ({
  users
})

export default connect(mapState, {
  createNote,
  fetchUsers
})(CreateNote)
