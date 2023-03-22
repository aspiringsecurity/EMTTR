import { defineStore } from 'pinia'

import axios from 'axios'
import TransportWebUsb from '@ledgerhq/hw-transport-webusb'
import IconService from 'icon-sdk-js'
import Icx from '@/assets/scripts/libs/hw-app-icx/Icx'
import { serializeTransaction } from '@/assets/scripts/helpers'
import { useUserStore } from '@/stores/user'

const network = ref<string>('Lisbon')
const isTestnet = network.value === 'Lisbon'
const url = isTestnet ? 'https://lisbon.net.solidwallet.io/' : 'https://ctz.solidwallet.io/'
const nid = isTestnet ? '2' : '1'
const provider = new IconService.HttpProvider(`${url}api/v3`)
const iconService = new IconService(provider)

type LedgerStatus = {
  isFetching: boolean
  currentPage: number
  error: string | null
}

type LedgerAddressData = {
  id: number
  address: string
  balance: number
  path: string
  isLoading: boolean
}

type LedgerAddressesList = LedgerAddressData[]

export const useLedgerStore = defineStore('ledger-store', () => {
  const { emit, events } = useEventsBus()
  const { notify } = useNotificationToast()
  const { loginUser } = useUserStore()
  const ITEMS_PER_PAGE = 5 as const

  // States
  const addressPath = ref<string>('')

  const ledgerAddresses = ref<LedgerAddressesList>([])
  const ledgerStatus = reactive<LedgerStatus>({
    isFetching: true,
    currentPage: 0,
    error: '',
  })

  // Actions
  // TODO: check type
  const BROADCAST_LEDGER_TX = async (txObj): Promise<string> => new Promise((resolve, reject) => {
    const options = {
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8',
      },
      data: txObj,
      url: `${url}api/v3`,
    }
    axios(options)
      .then((response) => {
        if (response.status === 200) {
          resolve(response.data.result)
        } else {
          reject(new Error('A JSONRPC error occured. It may be related to your balance or network condition.'))
        }
      })
      .catch(() => {
        reject(new Error('A JSONRPC error occured. It may be related to your balance or network condition.'))
      })
  })
  // TODO: check type
  const HANDLE_LEDGER_RPC = async (payload) => {
    try {
      const {
        value,
        from,
        to,
        data,
        stepLimit,
        timestamp,
      } = payload.params
      const transport = await TransportWebUsb.create()
      const icx = new Icx(transport)
      const storePath = addressPath.value
      const serialized = serializeTransaction([
        'icx_sendTransaction',
        ...[
          'data',
          [
            'method',
            data.method,
            'params',
            Object.entries(data.params)
              .sort(([a], [b]) => Number(a > b) - 0.5)
              .reduce((accu, curr) => [...accu, ...curr], []),
          ],
        ],
        ...['dataType', 'call'],
        ...['from', from],
        ...['nid', `0x${nid}`],
        ...['nonce', '0x1'],
        ...['stepLimit', stepLimit],
        ...['timestamp', timestamp],
        ...['to', to],
        ...value ? ['value', value] : [],
        ...['version', '0x3'],
      ])

      const signature = await icx.signTransaction(storePath, serialized)
      const txObj = { ...payload }
      txObj.params.signature = signature.signedRawTxBase64
      return await BROADCAST_LEDGER_TX(txObj)
    } catch (error) {
      throw new Error(error)
    }
  }
  // TODO: check type
  const HANDLE_LEDGER_SIGN = async (payload: { hash: string, from: string }) => {
    try {
      const transport = await TransportWebUsb.create()
      const icx = new Icx(transport)
      const storePath = addressPath.value
      const serialized = serializeTransaction([
        'icx_sendTransaction',
        ...[
          'data',
          [
            'method',
            'ledgerSign',
            'params',
            [
              'hash',
              payload.hash,
            ],
          ],
        ],
        ...['dataType', 'call'],
        ...['from', payload.from],
        ...['nid', '0x1'],
        ...['nonce', '0x1'],
        ...['stepLimit', '0x0'],
        ...['timestamp', '0x0'],
        ...['to', 'cx0000000000000000000000000000000000000000'],
        ...['version', '0x3'],
      ])
      const signature = await icx.signTransaction(storePath, serialized)

      return signature.signedRawTxBase64
    } catch (error) {
      throw new Error(error)
    }
  }
  // TODO: check type
  const dipsatchLedger = async ({ type, payload }) => {
    try {
      if (addressPath.value) {
        return await (
          type === 'REQUEST_JSON-RPC'
            ? HANDLE_LEDGER_RPC(payload)
            : HANDLE_LEDGER_SIGN(payload)
        )
      }
      throw new Error('Ledger Path error. Please log out and log in again')
    } catch (error) {
      throw new Error(error.statusCode === 27013 ? 'The transaction has been denied by the user' : error.message)
    }
  }
  const getLedgerAddresses = async (page: number): Promise<LedgerAddressesList> => {
    try {
      const transport = await TransportWebUsb.create()
      const icx = new Icx(transport)

      const ledgerBook: LedgerAddressesList = []
      for (let index = (ITEMS_PER_PAGE * page); index < ((ITEMS_PER_PAGE * page) + ITEMS_PER_PAGE); index++) {
        // eslint-disable-next-line no-await-in-loop
        const { address } = await icx.getAddress(`44'/4801368'/0'/0'/${index}'`)
        // eslint-disable-next-line no-await-in-loop
        const result = await iconService.getBalance(String(address)).execute()
        const balance = IconService.IconConverter.toNumber(result) / 10 ** 18
        ledgerBook.push({
          id: index,
          address: String(address),
          path: `44'/4801368'/0'/0'/${index}'`,
          balance,
          isLoading: false,
        })
      }

      return ledgerBook
    } catch (error) {
      throw new Error(error)
    }
  }
  const selectLedgerAddress = async <A extends LedgerAddressData>(address: A['address'], path: A['path']): Promise<void> => {
    const currentLedgerAddress = ledgerAddresses.value.find((ledgerAddress) => ledgerAddress.address === address)
    currentLedgerAddress.isLoading = true

    try {
      addressPath.value = path
      loginUser({ address, wallet: 'ledger' })
      emit(events.POPUP_CLOSE, { handlePending: true })
      notify.success({
        title: 'Log in successful',
        timeout: 5000,
      })
    } catch (error) {
      notify.error({
        title: 'Error',
        message: error,
        timeout: 5000,
      })
    } finally {
      currentLedgerAddress.isLoading = false
    }
  }
  const setLedgerPage = async (page: number): Promise<void> => {
    ledgerStatus.isFetching = true
    ledgerStatus.error = ''

    getLedgerAddresses(page)
      .then((result) => {
        ledgerAddresses.value = result
        ledgerStatus.currentPage = page
      })
      .catch((error) => {
        const stringError = String(error)
        let message = stringError
        if (stringError.includes('TransportOpenUserCancelled')) message = 'Ledger connection canceled.'
        else if (stringError.includes('TransportError')) message = 'Something wrong happened. Please retry later.'
        ledgerStatus.error = message
        notify.error({
          title: 'Error',
          message,
          timeout: 5000,
        })
      })
      .finally(() => {
        ledgerStatus.isFetching = false
      })
  }
  const selectNetwork = (net:string) => {
    network.value = net
  }

  return {
    // States
    addressPath,
    ledgerAddresses,
    ledgerStatus,
    network,

    // Actions
    dipsatchLedger,
    selectLedgerAddress,
    setLedgerPage,
    selectNetwork,
  }
}, { persist: true })
