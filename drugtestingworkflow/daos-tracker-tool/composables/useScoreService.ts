/* eslint-disable camelcase */
import axios from 'axios'
import IconService from 'icon-sdk-js'
import BigNumber from 'bignumber.js'
import { useLedgerStore } from '@/stores/ledger'

type Hash = string | BigNumber | number

type TransactionResult = {
  status: Hash;
  to: string;
  txHash: string;
  txIndex: number;
  blockHeight: number;
  blockHash: string;
  cumulativeStepUsed: BigNumber;
  stepUsed: BigNumber;
  stepPrice: BigNumber;
  scoreAddress?: string;
  eventLogs?: unknown;
  logsBloom?: unknown;
  failure?: unknown;
}

type ScoreParams = {
  [key in `_${string}`]: string | number | ScoreParams
}

export type BlockData = {
  block_hash: string
  confirmed_transaction_list: Array<{
    data?: {
      method?: string
      params?: ScoreParams
      result?: unknown
    }
    dataType: string
    timestamp: string
    txHash: string
    version: string
    from?: string
    nid?: string
    nonce?: string
    signature?: string
    stepLimit?: string
    to?: string
  }>
  height: number
  merkle_tree_root_hash: string
  peer_id: string
  prev_block_hash: string
  signature: string
  time_stamp: number
  version: string
}

export type TxResult = {
  result?: {
    blockHash: string
    blockHeight: string
    eventLogs: unknown[]
    status: string
    to: string
  }
  error?: {
    code: number
    message: string
  }
}

export const useScoreService = () => {
  const { scoreAddress } = useRuntimeConfig()
  const ledgerStore = useLedgerStore()
  const iconNetwork = ledgerStore.network
  const isTestnet: boolean = iconNetwork === 'Lisbon'
  const url = isTestnet ? 'https://lisbon.net.solidwallet.io/' : 'https://ctz.solidwallet.io/'
  const debug = isTestnet ? 'https://lisbon.net.solidwallet.io/api/v3d' : 'https://ctz.solidwallet.io/api/debug/v3'
  const nid = isTestnet ? '2' : '1'

  const service = new IconService(new IconService.HttpProvider(`${url}api/v3`))

  const SCORECallReadOnly = async <T>(method: string, params?: ScoreParams, score?: string): Promise<T> => {
    try {
      const txObj = new IconService.IconBuilder.CallBuilder()
        .to(score || scoreAddress)
        .method(method)
      const call = params ? txObj.params(params) : txObj

      return await service
        .call(call.build())
        .execute()
    } catch (error) {
      throw new Error(error)
    }
  }

  const getBlockData = async (blockHash: string): Promise<BlockData> => {
    try {
      const options = {
        method: 'post',
        url: `${url}api/v3`,
        data: {
          jsonrpc: '2.0',
          method: 'icx_getBlockByHash',
          id: 1234,
          params: {
            hash: blockHash,
          },
        },
      }
      return (await axios(options)).data.result
    } catch (error) {
      throw new Error(error)
    }
  }

  const getStepLimit = async (from: string, method: string, score: string, params?: { [key in string]: any }, value?: number): Promise<Hash> => {
    try {
      const stepLimit = await axios.post(debug, {
        id: 1234,
        jsonrpc: '2.0',
        method: 'debug_estimateStep',
        params: {
          from,
          data: {
            method,
            params: params || null,
          },
          dataType: 'call',
          nid: `0x${parseFloat(nid).toString(16)}`,
          nonce: '0x1',
          timestamp: `0x${((new Date()).getTime() * 1000).toString(16)}`,
          to: score,
          value: value ? `0x${(value * (10 ** 18)).toString(16)}` : null,
          version: '0x3',
        },
      })

      return stepLimit.data.result.toString() as Hash
    } catch (error) {
      throw error.response.data.error.message
    }
  }

  const getTxResult = async (hash: Hash): Promise<TransactionResult> => {
    try {
      return await service.getTransactionResult(hash).execute()
    } catch (error) {
      throw new Error(error)
    }
  }

  return {
    SCORECallReadOnly,
    getBlockData,
    getStepLimit,
    getTxResult,
  }
}
