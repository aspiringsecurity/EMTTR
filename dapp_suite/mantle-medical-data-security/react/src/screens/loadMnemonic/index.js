import LoadMnemonic from './LoadMnemonic'
import { connect } from 'react-redux'
import { loadMnemonic } from 'model/auth'

export default connect(null, {
  loadMnemonic
})(LoadMnemonic)
