const { healthcheck: abHealthcheck } = require('@appliedblockchain/koa-healthcheck')
const config = require('config')

const GIT_COMMIT_SHA_DEFAULT = 'No commit was passed into this build'
const GIT_TAG_DEFAULT = 'There was no tag associated with this build'
const HEALTH_TOKEN = process.env.HEALTH_TOKEN || config.get('HEALTH_TOKEN')

const getLatestBlock = async (web3) => {
  try {
    const lastBlockNumber = await web3.eth.getBlockNumber()
    return {
      lastBlockNumber,
      status: 'Running'
    }
  } catch (err) {
    return {
      lastBlockNumber: 0,
      status: err.message
    }
  }
}

// GIT_COMMIT_SHA and GIT_TAG are populated when creating the docker image
const healthcheck = (contractAddresses, web3) => {
  return async (ctx, next) => {
    const { lastBlockNumber, status } = await getLatestBlock(web3)
    const healthCheckMethod = abHealthcheck({
      custom: {
        storeContractAddress: contractAddresses,
        latestBlockNumber: lastBlockNumber,
        parityStatus: status,
        commit: process.env.GIT_COMMIT_SHA || GIT_COMMIT_SHA_DEFAULT,
        tag: process.env.GIT_TAG || GIT_TAG_DEFAULT
      }
    }, HEALTH_TOKEN)
    return healthCheckMethod(ctx, next)
  }
}


module.exports = {
  healthcheck,
  GIT_COMMIT_SHA_DEFAULT,
  GIT_TAG_DEFAULT
}
