const createServer = require('src/server')

const setupAppForTest = async () => createServer()

module.exports = {
  setupAppForTest
}
