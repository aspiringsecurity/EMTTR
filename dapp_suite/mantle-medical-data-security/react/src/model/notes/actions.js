import Mantle from '@appliedblockchain/mantle-core'
import api from 'utils/api'
import { CREATE_NOTE, FETCH_NOTES } from './reducer'

const { bufferToHex0x } = Mantle.utils

/* ACTION CREATORS */
export const fetchNotes = () => {
  return async (dispatch, getState) => {
    const { auth: { publicKey, mnemonic } } = getState()
    const { data } = await api.get('/notes')

    const mantle = new Mantle()
    mantle.loadMnemonic(mnemonic)

    const notes = data.map(note => {
      const { sharedWith, encryptedKeys, encrypted } = note
      const viewable = sharedWith.includes(publicKey)
      /**
       * @NOTE: `encryptedKeys` and `sharedWith` act as parallel arrays, i.e. index i in `sharedWith` (the public key)
       * corresponds to index i in `encryptedKeys` (the encrypted symmetric key for a user).
       *
       * @TODO: Look into better options. This seems like a possible pitfall (e.g. arrays cannot be modified without
       * first considering the impact on the related array). Solidity does not seem to provide a standard way of storing
       * key:value data but there should be a more reliable option
       */
      const index = sharedWith.indexOf(publicKey)
      const key = viewable ? Mantle.decrypt(encryptedKeys[index], mantle.privateKey) : null
      const decrypted = viewable ? Mantle.decryptSymmetric(encrypted, key) : encrypted

      return { ...note, decrypted, viewable }
    })

    dispatch({
      type: FETCH_NOTES,
      payload: notes
    })
  }
}

export const createNote = ({ tag, msg, sharedWith = [] }) => {
  return async (dispatch, getState) => {
    try {
      const { auth: { publicKey, address } } = getState()

      const symmetricKey = Mantle.createSymmetricKey()
      const encrypted = bufferToHex0x(
        Mantle.encryptSymmetric(msg, symmetricKey)
      )

      sharedWith.push(publicKey)
      const encryptedKeys = sharedWith.map(publicKey => bufferToHex0x(
        Mantle.encrypt(symmetricKey, publicKey)
      ))

      await api.post('/notes', {
        params: [
          tag,
          encrypted,
          address,
          sharedWith,
          encryptedKeys
        ]
      })

      dispatch({
        type: CREATE_NOTE,
        payload: {
          tag,
          encrypted,
          encryptedKeys,
          publicKey,
          address
        }
      })
    } catch (err) {
      throw err
    }
  }
}
