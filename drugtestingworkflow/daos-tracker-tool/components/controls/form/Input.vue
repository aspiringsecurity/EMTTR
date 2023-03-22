<template>
  <label class="grid gap-20">
    <h3
      v-if="label"
      class="typo-title-m text-white"
    >
      {{ label }}
    </h3>
    <input
      v-model="model"
      class="
        px-24
        py-20
        typo-text-medium
        rounded-max
        text-white
        bg-transparent
        border-1
        border-grey-200
        transition-default
        duration-100
        hover:border-grey-100
        focus:border-grey-100
        active:border-grey-100
        placeholder:text-grey-100
      "
      v-bind="$attrs"
    >
  </label>
</template>

<script setup lang="ts">
type ModelValue = string | number

type Emits = {
  (event: 'update:modelValue', value: ModelValue): void
}

type Props = {
  modelValue?: ModelValue
  label?: string
}

const emit = defineEmits<Emits>()
const props = defineProps<Props>()

const model = computed<ModelValue>({
  get() {
    return props.modelValue
  },
  set(value) {
    emit('update:modelValue', value)
  },
})
</script>
