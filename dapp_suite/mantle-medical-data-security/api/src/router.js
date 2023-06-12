const koaRouter = require('koa-joi-router')
const { API_PREFIX } = require('./constants')
const routes = require('src/routes')
const { parityProxy } = require('@appliedblockchain/mantle-api')

const router = koaRouter()

router.prefix(API_PREFIX)

routes.forEach(route => router.use('', route))

router.use('', parityProxy.createRouter().middleware())

module.exports = {
  middleware: router.middleware()
}
