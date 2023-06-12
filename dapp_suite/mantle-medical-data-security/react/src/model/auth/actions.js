import { createUser } from '../users'
import { LOAD_MNEMONIC } from './reducer'

/* ACTION CREATORS */
export const loadMnemonic = (mnemonic, newUser) => {
  return async dispatch => {
    dispatch({
      type: LOAD_MNEMONIC,
      payload: mnemonic
    })

    if (newUser) {
      dispatch(createUser(mnemonic))
    }
  }
}
