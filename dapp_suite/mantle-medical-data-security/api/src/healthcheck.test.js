const request = require('supertest')
const { setupAppForTest } = require('test/utils')
const { GIT_COMMIT_SHA_DEFAULT, GIT_TAG_DEFAULT } = require('./healthcheck')
const { contracts } = require('src/utils/web3')

const contractAddresses = [
  contracts.Notes.options.address,
  contracts.Users.options.address
]

let app

const URL = '/health'

describe(`GET ${URL}`, () => {
  beforeAll(async () => {
    app = await setupAppForTest()
  })

  afterAll(async () => {
    await app.close()
  })

  it('responds with the current contract address', async () => {
    const { body } = await request(app)
      .get(URL)
      .expect(200)

    expect(body.parityStatus).toEqual('Running')
    expect(body.storeContractAddress).toEqual(contractAddresses)
    expect(body.commit).toEqual(GIT_COMMIT_SHA_DEFAULT)
    expect(body.tag).toEqual(GIT_TAG_DEFAULT)
    expect(body.latestBlockNumber).toBeGreaterThan(0)
  })
})
