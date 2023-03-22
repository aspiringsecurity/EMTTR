<template>
  <component
    :is="to ? NuxtLink : 'button'"
    class="px-24 py-20 rounded-max transition-default duration-100"
    :class="{
      'typo-button-l text-white bg-grey-200 border-1 border-grey-200 hover:border-primary': version === 'primary',
      'typo-title-m text-white border-1 border-grey-200 hover:border-grey-100': version === 'secondary',
    }"
    v-bind="{
      ...$attrs,
      ...Object.entries({
        to,
        target,
        type,
      })
        .filter(([_, value]) => !!value)
        .reduce((accu, [key, value]) => ({ ...accu, ...!!value && { [key]: value } }), {}),
    }"
  >
    <slot />
  </component>
</template>

<script setup lang="ts">
import type { NuxtLinkProps } from '#app'

type Version =
  | 'primary'
  | 'secondary'

type Props = {
  version?: Version
  to?: NuxtLinkProps
  target?: '_self' | '_blank' | '_parent' | '_top'
  type?: 'submit' | 'reset' | 'button'
}

withDefaults(defineProps<Props>(), {
  version: 'primary',
})

const { NuxtLink } = useNuxtLink()
</script>
