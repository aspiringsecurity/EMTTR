import Mantle from '@appliedblockchain/mantle-core'

/* ACTION NAMES */
export const LOAD_MNEMONIC = '@app/loadMnemonic'

const INITIAL_STATE = {
  mnemonic: '',
  publicKey: '',
  address: ''
}

const reducer = (state = INITIAL_STATE, action) => {
  switch (action.type) {
    case LOAD_MNEMONIC: {
      const mnemonic = action.payload

      const mantle = new Mantle()
      mantle.loadMnemonic(mnemonic)

      const publicKey = mantle.getPublicKey('hex0x')
      const address = mantle.address

      return { ...state, mnemonic, publicKey, address }
    }
    default:
      return state
  }
}

export default reducer
