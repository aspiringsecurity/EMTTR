const fs = require('fs')
const { join } = require('path')
const Web3 = require('web3')
const config = require('config')
const Mantle = require('@appliedblockchain/mantle-core')
const PROVIDER = config.get('provider')
const FROM = config.get('from')

const directory = join(__dirname, '../build/contracts')
const files = fs
  .readdirSync(directory)
  .filter(d => /\.json$/.test(d))

const contracts = {}
files.forEach(file => {
  const filePath = join(directory, file)
  const fileContent = fs.readFileSync(filePath)

  try {
    const contract = JSON.parse(fileContent)
    contracts[contract.contractName] = contract
  } catch (err) {
    console.error(`Cannot parse file: ${file}`)
    process.exit(1)
  }
})

;(async () => {
  try {
    const web3 = new Web3(new Web3.providers.HttpProvider(PROVIDER))
    const coinbase = await web3.eth.getCoinbase()
    const from = FROM || coinbase
    const sendParams = { from, gas: 50000000 }

    const deployContract = async (contracts, contractName) => {
      const { abi, bytecode } = contracts[contractName]
      const contract = new web3.eth.Contract(abi, { from, data: bytecode })
      return contract.deploy({ arguments: [] }).send(sendParams)
    }

    const Notes = await deployContract(contracts, 'Notes')
    const Users = await deployContract(contracts, 'Users')

    const contractsJSON = `module.exports = ${
      JSON.stringify({
        Notes: { address: Notes.options.address, abi: Notes._jsonInterface },
        Users: { address: Users.options.address, abi: Users._jsonInterface }
      }, null, 2).replace(/"/g, '\'')
    }\n`

    // Generate default users for demonstration purposes

    const user1 = new Mantle()
    user1.loadMnemonic('exchange stove lunar piece impact range fall ketchup night hunt resource burden')

    const user2 = new Mantle()
    user2.loadMnemonic('license dish memory enemy mammal alley garage edge supreme join rival tree')

    await Users.methods
      .addUser(user1.address, user1.getPublicKey('hex0x'), 'User 1')
      .send(sendParams)

    await Users.methods
      .addUser(user2.address, user2.getPublicKey('hex0x'), 'User 2')
      .send(sendParams)

    const path = join(__dirname, '../../api/contracts/index.js')
    fs.writeFileSync(path, contractsJSON)
    console.log(`Contract information saved at ${path}`)
  } catch (err) {
    err.message === 'Invalid JSON RPC response: ""'
      ? console.error('Error: Unable to connect to network, is parity running?')
      : console.error(err)
  }
})()
