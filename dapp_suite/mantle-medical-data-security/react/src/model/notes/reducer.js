/* ACTION NAMES */
export const CREATE_NOTE = '@app/createNote'
export const FETCH_NOTES = '@app/fetchNotes'

const INITIAL_STATE = []

const reducer = (state = INITIAL_STATE, action) => {
  switch (action.type) {
    case CREATE_NOTE:
      return [ ...state, action.payload ]
    case FETCH_NOTES:
      return action.payload
    default:
      return state
  }
}

export default reducer
