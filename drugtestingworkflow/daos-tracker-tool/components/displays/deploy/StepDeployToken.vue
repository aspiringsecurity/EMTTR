<template>
  <div class="grid gap-10">
    <h1 class="typo-title-l text-white">
      Deploy your token
    </h1>
    <p class="typo-text-large text-grey-100">
      Your DAO will require a token to represent voting power, pick a token and fill out the parameters. Pick an option to discover the features of each token.
    </p>
  </div>
  <ControlsFormSelect
    v-model="models.token"
    label="Select token type"
    placeholder="Select a token"
    :options="['Soulbounds NFT']"
  />
  <span v-if="models.token == 'Soulbounds NFT'">
    SoulBounds NFT are untransferable NFT tokens. Soulbound tokens cannot be bought and sold and are not designed to have market value. Instead, they can be issued by individuals or by another entity to symbolize an accomplishment.
  </span>
  <!-- <div class="grid s:grid-cols-2 gap-16">
    <ControlsFormInput
      v-model="models.name"
      label="Name"
      placeholder="Name"
    />
    <ControlsFormInput
      v-model="models.supply"
      label="Total supply"
      placeholder="Total supply"
    />
  </div> -->
  <ControlsButtonAction @click="onDeployToken">
    Deploy token
  </ControlsButtonAction>
</template>

<script setup lang="ts">
import { useUserStore } from '@/stores/user'

type NextStep = 'StepDeployAgora'
const userStore = useUserStore()
const { setUserContracts } = userStore
type Props = {
  stepData?: string
}

type Emits = {
  (event: 'updateStep', parameter: { step: NextStep, data: string }): void
}

const emitStep = defineEmits<Emits>()
withDefaults(defineProps<Props>(), {
  stepData: '',
})

const { emit, events } = useEventsBus()
const { notify } = useNotificationToast()

const models = reactive<{ token: string }>({ token: '' })

const onDeployToken = (): void => {
  if (models.token !== '') {
    emit(events.POPUP_ACTION, {
      name: 'DeployToken',
      params: { type: 'soulbound' },
      handleGuard: true,
      onClose: (returnData) => {
        if (typeof returnData === 'object' && returnData !== null && 'scoreAddress' in returnData && typeof returnData.scoreAddress === 'string') {
          setUserContracts('token', returnData.scoreAddress)
          emitStep('updateStep', { step: 'StepDeployAgora', data: returnData.scoreAddress })
        }
      },
    })
  } else {
    notify.error({
      title: 'Warning',
      message: 'You need to select a token type',
      timeout: 5000,
    })
  }
}
</script>
