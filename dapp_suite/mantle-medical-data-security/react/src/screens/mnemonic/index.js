import Mnemonic from './Mnemonic'
import { connect } from 'react-redux'

const mapStateToProps = ({ auth: { mnemonic } }) => ({ mnemonic })

export default connect(mapStateToProps)(Mnemonic)
