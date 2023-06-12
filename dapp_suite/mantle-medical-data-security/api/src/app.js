const createServer = require('./server')
const config = require('config')

const PORT = config.get('PORT')

const runApp = async () => {
  const app = await createServer()
  await app.listen(PORT)
}

module.exports = runApp

