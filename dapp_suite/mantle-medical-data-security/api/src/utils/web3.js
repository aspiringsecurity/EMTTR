const Web3 = require('web3')
const config = require('config')
const ETHEREUM_JSONRPC_ENDPOINT = config.get('ETHEREUM_JSONRPC_ENDPOINT')

const contractsJSON = require('contracts')

const web3 = new Web3(ETHEREUM_JSONRPC_ENDPOINT)
const Notes = new web3.eth.Contract(contractsJSON.Notes.abi, contractsJSON.Notes.address)
const Users = new web3.eth.Contract(contractsJSON.Users.abi, contractsJSON.Users.address)

const contracts = { Notes, Users }

const checkDeployment = async () => {
  const errors = []
  Object.entries(contractsJSON).forEach(async ([ _, { address } ]) => {
    const code = await web3.eth.getCode(address)
    if (code === '0x0' || code === '0x') {
      errors.push(new Error(`No code at the specified contract address: ${address}`))
    }
  })

  if (errors.length) {
    throw errors
  }
}

module.exports = {
  web3,
  contracts,
  checkDeployment
}
