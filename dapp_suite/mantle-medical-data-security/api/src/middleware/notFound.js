'use strict'

module.exports = async ({ request, notFound }) => {
  const { method, path } = request

  const message = `${method} ${path}`

  notFound({
    message: `No endpoint matched your request: ${message}`
  })
}
