<template>
  <PartialsPopup require-button>
    <template #header>
      <h2>
        <client-only>
          <template v-if="ACTION_SETAGORA.tx.hash">
            Your Agora contract is set!
          </template>
          <template v-else-if="ACTION_SETAGORA.isLoading || ACTION_SETAGORA.isWaiting">
            Setting...
          </template>
        </client-only>
      </h2>
    </template>
    <template #body>
      <transition
        name="fade-bounce"
        mode="out-in"
      >
        <!-- SUCCESS -->
        <div
          v-if="ACTION_SETAGORA.tx.hash"
          key="success"
          class="grid gap-20 typo-paragraph"
        >
          <span>
            You can now deploy the Agora UI

          </span>
          <ControlsButtonAction @click="closePopup">
            Next
          </ControlsButtonAction>
        </div>
        <!-- LOADING -->
        <div
          v-else-if="ACTION_SETAGORA.isLoading || ACTION_SETAGORA.isWaiting"
          key="loading"
          class="grid gap-20 typo-paragraph"
        >
          <span>
            Agora is being set. Please wait for few seconds.
          </span>
        </div>
      </transition>
    </template>
  </PartialsPopup>
</template>

<script setup lang="ts">
import IconService from 'icon-sdk-js'
import { storeToRefs } from 'pinia'
import type { BlockData } from '@/composables/useScoreService'
import { useLedgerStore } from '@/stores/ledger'
import { useUserStore } from '@/stores/user'
import soulbound from '@/public/contract_hash/soulbound'
import agora from '@/public/contract_hash/agora'

const { IconConverter, IconBuilder } = IconService
const { CallTransactionBuilder } = IconBuilder

type ActionData = {
  type: string
  tx: Record<string, unknown>
  query: Record<string, unknown>
  isListening: boolean
  isWaiting: boolean
  isLoading: boolean
  isSuccess: boolean
}

type Query = {
  jsonrpc: string
  method: string
  params: ReturnType<typeof IconConverter.toRawTransaction>
  id: number
}

const { network } = storeToRefs(useLedgerStore())
const route = useRoute()
const uid = route?.params?.uid

const { emit, bus, events } = useEventsBus()
const { getBlockData, getTxResult, getStepLimit } = useScoreService()
const { notify } = useNotificationToast()
const { ICONEX_HANDLE_CANCEL } = useIconexListener()

const { dipsatchLedger } = useLedgerStore()
const { address, wallet } = storeToRefs(useUserStore())

const nid = network.value === 'Lisbon' ? '2' : '1'
type Props = {
  address: string,
  agora:string,
  tokenId: string
}

const props = defineProps<Props>()

const isGlobalListening = ref<boolean>(false)
const ACTION_SETAGORA = reactive<ActionData>({
  type: 'RPC',
  tx: {},
  query: {},
  isListening: false,
  isWaiting: false,
  isLoading: false,
  isSuccess: false,
})

const getSetQuery = async (): Promise<Query> => {
  try {
    const params = {
      _address: props.address,
      _type: props.tokenId ? 'irc-31' : 'irc-2',
    }
    if (props.tokenId) params._id = props.tokenId

    const tx = new CallTransactionBuilder()
      .from(address.value)
      .to(props.agora)
      .stepLimit(IconConverter.toBigNumber('3000000000'))
      .nid(IconConverter.toBigNumber(nid))
      .nonce(IconConverter.toBigNumber('1'))
      .version(IconConverter.toBigNumber('3'))
      .timestamp((new Date()).getTime() * 1000)
      .method('setGovernanceToken')
      .params(params)
      .build()

    return {
      jsonrpc: '2.0',
      method: 'icx_sendTransaction',
      params: IconConverter.toRawTransaction(tx),
      id: 1198,
    }
  } catch (error) {
    notify.error({
      title: 'Error',
      message: error,
      timeout: 5000,
    })
    return null
  }
}

const makeSetQuery = async (hash: string): Promise<{ block: BlockData, tx: Awaited<ReturnType<typeof getTxResult>> }> => new Promise((resolve, reject) => {
  getTxResult(hash)
    .then((tx) => {
      if (tx.status === 1) {
        console.log(tx)
        getBlockData(tx.blockHash)
          .then((block) => {
            resolve({ block, tx })
          })
      } else {
        reject(tx.failure)
      }
    })
    .catch(() => {
      setTimeout(() => {
        resolve(makeSetQuery(hash))
      }, 2000)
    })
})

const RESET_SET = (): void => {
  ACTION_SETAGORA.tx = {}
  ACTION_SETAGORA.query = {}
  ACTION_SETAGORA.isListening = false
  ACTION_SETAGORA.isWaiting = false
  ACTION_SETAGORA.isLoading = false
  ACTION_SETAGORA.isSuccess = false
}

const RESET_LISTENER = (): void => {
  isGlobalListening.value = false
  RESET_SET()
}

const CALLBACK_SET = async (hash: string, scoreAddress: string): Promise<void> => {
  try {
    RESET_SET()
    ACTION_SETAGORA.tx = { hash, scoreAddress }
    ACTION_SETAGORA.isSuccess = true
    // scoreAddress
  } catch (error) {
    notify.error({
      title: 'Error',
      message: error,
      timeout: 5000,
    })
  }
}

const COMPLETE_SET = async (hash: string): Promise<void> => {
  try {
    ACTION_SETAGORA.isWaiting = false
    ACTION_SETAGORA.isLoading = true
    const { tx } = await makeSetQuery(hash)
    CALLBACK_SET(tx.txHash, tx.scoreAddress)
  } catch (error) {
    RESET_SET()

    notify.error({
      title: 'Error',
      message: error,
      timeout: 5000,
    })
  }
}

const HANDLE_RPC = async (payload): Promise<void> => {
  const { error, result } = payload.payload
  if (error) {
    RESET_LISTENER()

    notify.error({
      title: 'Error',
      message: error.message,
      timeout: 5000,
    })
  } else if (result) {
    isGlobalListening.value = false
    if (ACTION_SETAGORA.type === 'RPC' && ACTION_SETAGORA.isListening) {
      ACTION_SETAGORA.isListening = false
      await COMPLETE_SET(result)
    }
  }
}

const HANDLE_SIGN = ({ error = '', payload }): void => {
  if (error) {
    RESET_LISTENER()

    notify.error({
      title: 'Error',
      message: error,
      timeout: 5000,
    })
  } else if (payload) {
    isGlobalListening.value = false
  }
}

const TX_ROUTER = async ({ type, payload }: { type: string, payload: Query }): Promise<void> => {
  if (!wallet.value || wallet.value === 'iconex') {
    window.dispatchEvent(new CustomEvent('ICONEX_RELAY_REQUEST', {
      detail: { type, payload },
    }))
  } else if (wallet.value === 'ledger') {
    try {
      const result = await dipsatchLedger({ type, payload })
      if (type === 'REQUEST_JSON-RPC') {
        HANDLE_RPC({ payload: { result } })
      } else {
        HANDLE_SIGN({ payload: { result } })
      }
    } catch (error) {
      HANDLE_RPC({ payload: { error: error.message } })

      notify.error({
        title: 'Error',
        message: error.message,
        timeout: 5000,
      })
    }
  }
}

const DISPATCH_SET = async (): Promise<void> => {
  ACTION_SETAGORA.query = {
    address: address.value,
  }

  const query = await getSetQuery()

  isGlobalListening.value = true
  ACTION_SETAGORA.isWaiting = true
  ACTION_SETAGORA.isListening = true

  TX_ROUTER({ type: 'REQUEST_JSON-RPC', payload: query })
}

const closePopup = (): void => {
  const returnData = ACTION_SETAGORA.tx
  RESET_SET()
  emit(events.POPUP_CLOSE, { returnData })
}

watch(() => bus.value.get(events.ICONEX_CANCEL), () => {
  RESET_LISTENER()
  ICONEX_HANDLE_CANCEL({ error: 'Cancelled' })
})

watch(() => bus.value.get(events.ICONEX_RPC), HANDLE_RPC)

onMounted(async () => {
  await DISPATCH_SET()
})
</script>
