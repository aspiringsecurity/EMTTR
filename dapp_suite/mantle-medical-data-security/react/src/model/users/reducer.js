/* ACTION NAMES */
export const CREATE_USER = '@app/createUser'
export const FETCH_USERS = '@app/fetchUsers'

const INITIAL_STATE = []

const reducer = (state = INITIAL_STATE, action) => {
  switch (action.type) {
    case FETCH_USERS:
      return action.payload
    case CREATE_USER:
      return [ ...state, action.payload ]
    default:
      return state
  }
}

export default reducer
