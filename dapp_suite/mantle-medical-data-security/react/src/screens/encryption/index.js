import Encryption from './Encryption'
import { connect } from 'react-redux'

const mapStateToProps = ({ auth: { mnemonic } }) => ({
  mnemonic
})

export default connect(mapStateToProps)(Encryption)
