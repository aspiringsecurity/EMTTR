const runApp = require('src/app')
const logger = require('src/logger')
const config = require('config')

const NODE_ENV = config.get('NODE_ENV')
const PORT = config.get('PORT')

const { Notes, Users } = require('contracts')

;(async () => {
  try {
    if (!Notes.address || !Users.address) {
      throw new Error('You must start the server with a valid "Notes" and "Users" contract deployed')
    }

    await runApp()

    logger.debug(`Server listening on ${PORT} in '${NODE_ENV}' mode`)
    logger.debug(`Contract deployed at ${Notes.address}`)
    logger.debug(`Docs available at http://localhost:${PORT}/docs`)
  } catch (err) {
    logger.error('Error while starting up server')
    logger.error(err)

    process.exit(1)
  }
})()
