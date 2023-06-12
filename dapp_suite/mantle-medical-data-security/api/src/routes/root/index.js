const koaRouter = require('koa-joi-router')
const router = koaRouter()

const handler = async ctx => {
  ctx.ok({ message: 'hello world' })
}

router.route([
  {
    method: 'get',
    path: '/',
    handler
  }
])

module.exports = router.middleware()
