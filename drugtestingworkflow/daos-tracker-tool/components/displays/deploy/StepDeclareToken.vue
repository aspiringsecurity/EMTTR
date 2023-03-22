<template>
  <div class="grid gap-10">
    <h1 class="typo-title-l text-white">
      Get your DAO started
    </h1>
    <p class="typo-text-large text-grey-100">
      This page allows you to deploy the contracts required to launch a token based DAO on the ICON network. Do you have a token ready for your governance?
    </p>
  </div>
  <ControlsFormSelect
    v-model="models.network"
    label="Select a network"
    placeholder="Lisbon"
    :options="['Lisbon','Mainnet']"
  />
  <div class="grid m:grid-cols-2 gap-16">
    <ControlsButtonAction
      version="secondary"
      @click="triggerNextStep('StepDeployToken')"
    >
      I don't have a token contract
    </ControlsButtonAction>
    <ControlsButtonAction
      version="secondary"
      @click="triggerNextStep('StepDeployAgora')"
    >
      I have a token ready
    </ControlsButtonAction>
  </div>
</template>

<script setup lang="ts">
import { useLedgerStore } from '@/stores/ledger'

type NextStep = 'StepDeployToken' | 'StepDeployAgora'
const ledgerStore = useLedgerStore()
const { selectNetwork } = ledgerStore
type Props = {
  stepData?: string
}

type Emits = {
  (event: 'updateStep', parameter: { step: NextStep }): void
}
const models = reactive<{ network: string }>({ network: 'Lisbon' })
const emit = defineEmits<Emits>()
withDefaults(defineProps<Props>(), {
  stepData: '',

})

const triggerNextStep = (step:string): void => {
  console.log('trigger')
  selectNetwork(models.network)
  emit('updateStep', { step })
}

</script>
