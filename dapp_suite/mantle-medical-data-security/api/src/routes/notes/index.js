const koaRouter = require('koa-joi-router')
const Joi = koaRouter.Joi
const router = koaRouter()
const ignoreNumberedKeys = require('src/utils/ignoreNumberedKeys')
const { contracts: { Notes } } = require('src/utils/web3')

const routes = [
  {
    method: 'get',
    path: '/notes',
    output: {
      200: {
        body: Joi.array().items(Joi.object())
      }
    },
    handler: async ctx => {
      try {
        const count = await Notes.methods.getNoteCount().call()
        const promises = []
        for (let i = 0; i < count; i++) {
          const note = Notes.methods.getNote(i).call()
          promises.push(note)
        }

        const notes = (await Promise.all(promises)).map(ignoreNumberedKeys)
        ctx.ok(notes.reverse())
      } catch (error) {
        ctx.throw(error)
      }
    }
  },
  {
    method: 'post',
    path: '/notes',
    validate: {
      type: 'json',
      body: {
        params: Joi.array().items(
          Joi.string().required(),
          Joi.string().required(),
          Joi.string().required(),
          Joi.array().items(Joi.string()).required(),
          Joi.array().items(Joi.string()).required()
        )
      }
    },
    handler: async ctx => {
      try {
        const { params } = ctx.request.body
        await Notes.methods.addNote(...params).send()
        ctx.ok()
      } catch (error) {
        ctx.throw(error)
      }
    }
  }
]

router.route(routes)

module.exports = router
