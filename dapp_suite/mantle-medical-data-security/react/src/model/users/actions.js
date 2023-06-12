import Mantle from '@appliedblockchain/mantle-core'
import api from 'utils/api'
import { CREATE_USER, FETCH_USERS } from './reducer'

/* ACTION CREATORS */
export const createUser = mnemonic => {
  return async dispatch => {
    try {
      const mantle = new Mantle()
      mantle.loadMnemonic(mnemonic)

      const user = {
        addr: mantle.address,
        pubKey: mantle.getPublicKey('hex0x')
      }

      const { addr, pubKey } = user
      const { data } = await api.post('/users', {
        params: [
          addr,
          pubKey
        ]
      })

      dispatch({
        type: CREATE_USER,
        payload: data
      })
    } catch (err) {
      throw err
    }
  }
}

export const fetchUsers = () => {
  return async dispatch => {
    const { data: users } = await api.get('/users')

    dispatch({
      type: FETCH_USERS,
      payload: users
    })
  }
}
