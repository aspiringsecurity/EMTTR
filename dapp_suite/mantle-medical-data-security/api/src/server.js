const http = require('http')
const Koa = require('koa')
const cors = require('@koa/cors')
const compress = require('koa-compress')
const respond = require('koa-respond')
const { middleware } = require('./router')
const rootMiddleware = require('./routes/root')
const logger = require('./logger')
const { errorHandler, notFoundHandler } = require('./middleware')
const { healthcheck } = require('./healthcheck')
const { web3, contracts, checkDeployment } = require('src/utils/web3')

const createServer = async () => {
  logger.debug('Creating server...')

  await checkDeployment()

  const [ from ] = await web3.eth.getAccounts()
  const contractAddresses = []

  /**
   * Configure contract options
   * @TODO:
   * Configure contracts in the src/utils/web3 file. This approach of configuring them on server start-up
   * instead of on web3 instantiation is difficult to reason about
   */
  Object.keys(contracts).forEach(key => {
    const contract = contracts[key]
    contract.options = { ...contract.options, from, gas: 50000000 }
    contractAddresses.push(contract.options.address)
  })

  const app = new Koa()

  app
    .use(compress())
    .use(errorHandler)
    .use(respond())
    .use(cors())
    .use(healthcheck(contractAddresses, web3))
    .use(rootMiddleware)
    .use(middleware)
    .use(notFoundHandler)

  const server = http.createServer(app.callback())

  server.on('close', async () => {
    logger.debug('Server closing')
  })

  server.on('error', async error => {
    logger.debug('Error', error)
  })

  logger.debug('Server created.')

  return server
}

module.exports = createServer
