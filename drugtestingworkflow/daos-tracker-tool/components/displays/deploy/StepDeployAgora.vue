<template>
  <div class="grid gap-10">
    <h1 class="typo-title-l text-white">
      Deploy Agora
    </h1>
    <p class="typo-text-large text-grey-100">
      The next step is to deploy Agora, the governance framework for you DAO.
    </p>
  </div>
  <ControlsButtonAction @click="onDeployAgora">
    Deploy Agora
  </ControlsButtonAction>
  <div class="w-full h-1 bg-grey-200" />
  <div class="grid gap-10">
    <h1 class="typo-title-l text-white">
      Setup Agora contracts
    </h1>
    <p class="typo-text-large text-grey-100">
      Once Agora is deployed, you can set it to track balances from your token contract.
      <span v-if="models.token == 'Soulbounds NFT'">For a Soulbound NFT, you will need to specify the tokenId of the token that holds governance power. You can set tokenId as 1 if  are not sure of the tokenId to choose.</span>
    </p>
  </div>
  <ControlsFormSelect
    v-model="models.token"
    label="Select token type"
    placeholder="Select a token"
    :options="['IRC2 Token','Soulbounds NFT','Staked IRC2 token']"
  />
  <div class="grid s:grid-cols-2 gap-16">
    <ControlsFormInput
      v-model="models.address"
      label="Token address"
      placeholder="cx..."
    />
    <ControlsFormInput
      v-if="models.token == 'Soulbounds NFT' || models.token == 'Staked IRC2 token'"
      v-model="models.id"
      label="Id"
      placeholder="1"
    />
  </div>

  <ControlsButtonAction @click="onSetupAgora">
    Set Agora
  </ControlsButtonAction>
</template>

<script setup lang="ts">
import { useUserStore } from '@/stores/user'

const { notify } = useNotificationToast()

type NextStep = 'StepFinale'
const userStore = useUserStore()
const { setUserContracts } = userStore
type Props = {
  stepData?: string
}

type Emits = {
  (event: 'updateStep', parameter: { step: NextStep, data: string }): void
}

const emitStep = defineEmits<Emits>()
const props = withDefaults(defineProps<Props>(), {
  stepData: '',
})

const { emit, events } = useEventsBus()

const models = reactive<{ token: string, address: string, id: string }>({ token: '', address: props.stepData || '', id: null })
const agoraScore = ref<string>('')

const onDeployAgora = (): void => {
  emit(events.POPUP_ACTION, {
    name: 'DeployToken',
    params: { type: 'agora', variant: models.token },
    handleGuard: true,
    onClose: (returnData) => {
      if (typeof returnData === 'object' && returnData !== null && 'scoreAddress' in returnData && typeof returnData.scoreAddress === 'string') {
        console.log(returnData.scoreAddress)
        setUserContracts('agora', returnData.scoreAddress)
        agoraScore.value = returnData.scoreAddress
      }
    },
  })
}

const onSetupAgora = (): void => {
  if (agoraScore.value === '') {
    notify.error({
      title: 'Warning',
      message: 'You need to deploy an Agora SCORE first',
      timeout: 5000,
    })
  } else if (models.token === '') {
    notify.error({
      title: 'Warning',
      message: 'You need to select a token type',
      timeout: 5000,
    })
  } else if (models.token !== '' || models.address !== '') {
    const params = { address: models.address, agora: agoraScore.value, tokenId: null }
    if (models.id) params.tokenId = models.id
    emit(events.POPUP_ACTION, {
      name: 'SetAgora',
      params,
      handleGuard: true,
      onClose: (returnData) => {
        console.log('close')
        if (typeof returnData === 'object' && returnData !== null) {
          emitStep('updateStep', { step: 'StepFinale', data: returnData.scoreAddress })
        }
      },
    })
  } else {
    notify.error({
      title: 'Warning',
      message: 'You need to select a token type and address',
      timeout: 5000,
    })
  }
}
</script>
