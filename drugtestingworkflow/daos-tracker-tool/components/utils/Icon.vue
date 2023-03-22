<template>
  <component
    :is="icon"
    class="text-current inline-block transition-all duration-200"
    v-bind="$attrs"
  />
</template>

<script setup lang="ts">
import type { IconsNames, IconsComponents } from '@/composables/useIconsComponents'

type Props = {
  name: IconsNames
}

const icons = useIconsComponents()

const props = defineProps<Props>()

const icon = computed<IconsComponents>(() => icons[props.name])
</script>
