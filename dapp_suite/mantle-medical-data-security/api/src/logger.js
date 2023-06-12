const { APP_NAME } = require('./constants')
const { createLogger, WARN } = require('bunyan')
const config = require('config')
const LOG_LEVEL = config.get('LOG_LEVEL')

module.exports = createLogger({
  name: APP_NAME,
  level: LOG_LEVEL || WARN
})
