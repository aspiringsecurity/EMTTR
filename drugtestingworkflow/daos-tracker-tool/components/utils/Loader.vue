<template>
  <div
    :class="[
      $style.loader,
      {
        'w-20 h-20': size === 20,
        'w-40 h-40': size === 40,
        'border-1': thickness === 1,
        'border-2': thickness === 2,
        'border-4': thickness === 4,
      }
    ]"
    class="inline-block text-current rounded-full border-solid border-transparent border-t-current animate-spin will-change-transform"
  />
</template>

<script setup lang="ts">
type Size = 20 | 40

type Thickness = 1 | 2 | 4

type Props = {
  size?: Size
  thickness?: Thickness
}

withDefaults(defineProps<Props>(), {
  size: 20,
  thickness: 1,
})
</script>

<style lang="scss" module>
.loader {
  animation-timing-function: cubic-bezier(.5, 0, 0, .75);
}
</style>
