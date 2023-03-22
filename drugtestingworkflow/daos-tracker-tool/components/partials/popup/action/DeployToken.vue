<template>
  <PartialsPopup require-button>
    <template #header>
      <h2>
        <client-only>
          <template v-if="ACTION_DEPLOYTOKEN.tx.hash">
            Your contract have been deployed!
          </template>
          <template v-else-if="ACTION_DEPLOYTOKEN.isLoading || ACTION_DEPLOYTOKEN.isWaiting">
            Deploying...
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
          v-if="ACTION_DEPLOYTOKEN.tx.hash"
          key="success"
          class="grid gap-20 typo-paragraph"
        >
          <span>
            The contract has been deployed at this address : {{ ACTION_DEPLOYTOKEN.tx.scoreAddress }}

          </span>
          <ControlsButtonAction @click="closePopup">
            Next
          </ControlsButtonAction>
        </div>
        <!-- LOADING -->
        <div
          v-else-if="ACTION_DEPLOYTOKEN.isLoading || ACTION_DEPLOYTOKEN.isWaiting"
          key="loading"
          class="grid gap-20 typo-paragraph"
        >
          <span>
            The contract is being deployed. Please wait for few seconds.
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
import agoraSoulbound from '~/public/contract_hash/agora-soulbound'

const { IconConverter, IconBuilder } = IconService
const { DeployTransactionBuilder } = IconBuilder

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
  type: string,
  variant?: string
}

const props = defineProps<Props>()

const isGlobalListening = ref<boolean>(false)
const ACTION_DEPLOYTOKEN = reactive<ActionData>({
  type: 'RPC',
  tx: {},
  query: {},
  isListening: false,
  isWaiting: false,
  isLoading: false,
  isSuccess: false,
})

let content

if (props.type === 'soulbound') {
  content = soulbound
} else if (props.type === 'agora') {
  if (props.variant === 'Soulbounds NFT') {
    content = agoraSoulbound
    console.log('variant')
  } else {
    content = agora
    console.log('classic')
  }
}

const getDeployQuery = async (): Promise<Query> => {
  try {
    const tx = new DeployTransactionBuilder()
      .from(address.value)
      .to('cx0000000000000000000000000000000000000000')
      .stepLimit(IconConverter.toBigNumber('3000000000'))
      .nid(IconConverter.toBigNumber(nid))
      .nonce(IconConverter.toBigNumber('1'))
      .version(IconConverter.toBigNumber('3'))
      .timestamp((new Date()).getTime() * 1000)
      .contentType('application/java')
      .content(content)
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

const makeDeployQuery = async (hash: string): Promise<{ block: BlockData, tx: Awaited<ReturnType<typeof getTxResult>> }> => new Promise((resolve, reject) => {
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
        resolve(makeDeployQuery(hash))
      }, 2000)
    })
})

const RESET_DEPLOY = (): void => {
  ACTION_DEPLOYTOKEN.tx = {}
  ACTION_DEPLOYTOKEN.query = {}
  ACTION_DEPLOYTOKEN.isListening = false
  ACTION_DEPLOYTOKEN.isWaiting = false
  ACTION_DEPLOYTOKEN.isLoading = false
  ACTION_DEPLOYTOKEN.isSuccess = false
}

const RESET_LISTENER = (): void => {
  isGlobalListening.value = false
  RESET_DEPLOY()
}

const CALLBACK_DEPLOY = async (hash: string, scoreAddress: string): Promise<void> => {
  try {
    RESET_DEPLOY()
    ACTION_DEPLOYTOKEN.tx = { hash, scoreAddress }
    ACTION_DEPLOYTOKEN.isSuccess = true
    // scoreAddress
  } catch (error) {
    notify.error({
      title: 'Error',
      message: error,
      timeout: 5000,
    })
  }
}

const COMPLETE_DEPLOY = async (hash: string): Promise<void> => {
  try {
    ACTION_DEPLOYTOKEN.isWaiting = false
    ACTION_DEPLOYTOKEN.isLoading = true
    const { tx } = await makeDeployQuery(hash)
    CALLBACK_DEPLOY(tx.txHash, tx.scoreAddress)
  } catch (error) {
    RESET_DEPLOY()

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
    if (ACTION_DEPLOYTOKEN.type === 'RPC' && ACTION_DEPLOYTOKEN.isListening) {
      ACTION_DEPLOYTOKEN.isListening = false
      await COMPLETE_DEPLOY(result)
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

const DISPATCH_DEPLOY = async (): Promise<void> => {
  ACTION_DEPLOYTOKEN.query = {
    address: address.value,
  }

  const query = await getDeployQuery()

  isGlobalListening.value = true
  ACTION_DEPLOYTOKEN.isWaiting = true
  ACTION_DEPLOYTOKEN.isListening = true

  TX_ROUTER({ type: 'REQUEST_JSON-RPC', payload: query })
}

const closePopup = (): void => {
  const returnData = ACTION_DEPLOYTOKEN.tx
  RESET_DEPLOY()
  emit(events.POPUP_CLOSE, { returnData })
}

watch(() => bus.value.get(events.ICONEX_CANCEL), () => {
  RESET_LISTENER()
  ICONEX_HANDLE_CANCEL({ error: 'Cancelled' })
})

watch(() => bus.value.get(events.ICONEX_RPC), HANDLE_RPC)

onMounted(async () => {
  await DISPATCH_DEPLOY()
})
</script>
