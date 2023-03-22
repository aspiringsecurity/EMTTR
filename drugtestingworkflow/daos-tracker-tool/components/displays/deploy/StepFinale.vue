<template>
  <div class="grid gap-10">
    <h1 class="typo-title-l text-white">
      Congratulations! 🎉
    </h1>
    <p class="typo-text-large text-grey-100">
      You have deployed the on-chain infrastructure for your DAO 🚀 The next step consist in deploying the user interface for Agora, you can do so by following the instructions there: <a
        target="blank"
        class="underline"
        href="https://github.com/Staky-io/agora"
      >https://github.com/Staky-io/agora</a>
    </p>
    <p class="text-grey-100">
      Your contracts can be found here:
      <span
        v-if="userContracts.token"
        class="block mt-6"
      >
        <h3>Token:</h3>
        <a
          class="underline"
          :href="
            explorerBase+userContracts.token"
        >{{ userContracts.token }}</a>
      </span>
      <span
        v-if="userContracts.agora"
        class="block mt-6"
      >
        <h3>Agora:</h3>
        <a
          class="underline"
          :href="
            explorerBase+userContracts.agora"
        >{{ userContracts.agora }}</a>
      </span>
    </p>
  </div>
</template>

<script setup lang="ts">
import { useUserStore } from '@/stores/user'
import { useLedgerStore } from '@/stores/ledger'
import { storeToRefs } from 'pinia'

type NextStep = 'StepDeployToken' | 'StepDeployAgora'

const { userContracts } = storeToRefs(useUserStore())
const { network } = storeToRefs(useLedgerStore())
const explorerBase = network.value === 'Lisbon' ? 'https://lisbon.tracker.solidwallet.io/contract/' : 'https://tracker.icon.foundation/contract/'
type Props = {
  stepData?: string
}

type Emits = {
  (event: 'updateStep', parameter: { step: NextStep }): void
}

const emit = defineEmits<Emits>()
withDefaults(defineProps<Props>(), {
  stepData: '',
})
</script>
