<template>
  <div class="relative grid grid-flow-col gap-20 items-center justify-between">
    <h3
      v-if="label"
      class="typo-caption text-white uppercase"
    >
      {{ label }}
    </h3>
    <button
      class="
        grid
        grid-flow-col
        gap-10
        items-center
        px-16
        py-12
        typo-text-medium
        text-white
        bg-grey-200
        border-1
        border-grey-200
        rounded-max
        transition-default
        duration-100
        hover:border-primary
      "
      @click="isSelecting = !isSelecting"
    >
      {{ model || placeholder }}
      <UtilsIcon
        name="Chevron/Down"
        class="w-20 h-20 text-[#A1A4AC]"
      />
    </button>
    <Teleport to="#root">
      <transition name="fade">
        <div
          v-if="isSelecting"
          class="fixed top-0 left-0 w-screen h-screen bg-grey-400 bg-opacity-75"
          @click="isSelecting = false"
        />
      </transition>
      <div class="flex items-center justify-center fixed top-0 left-0 w-screen h-screen pointer-events-none">
        <transition name="fade-up">
          <div
            v-if="isSelecting"
            class="grid gap-16 w-224 p-20 bg-grey-300 rounded-20 pointer-events-auto"
          >
            <div class="grid grid-flow-col gap-10 justify-between items-center">
              <h3 class="typo-title-s text-white">
                Select a token
              </h3>
              <button
                class="text-grey-100 hover:text-white"
                @click="isSelecting = false"
              >
                <UtilsIcon
                  name="Cross"
                  class="w-20 h-20"
                />
              </button>
            </div>
            <div class="grid gap-10">
              <button
                v-for="(option, i) in options"
                :key="`option-${i}`"
                class="px-10 py-12 rounded-max typo-button-s text-white text-left border-1 border-grey-200 opacity-20 transition-default duration-100 hover:opacity-100 hover:border-primary"
                :class="{ 'opacity-100 border-primary': option === model }"
                @click="select(option)"
              >
                {{ option }}
              </button>
            </div>
          </div>
        </transition>
      </div>
    </Teleport>
  </div>
</template>

<script setup lang="ts">
type ModelValue = string | number

type Emits = {
  (event: 'update:modelValue', value: ModelValue): void
}

type Props = {
  modelValue?: ModelValue
  label?: string
  placeholder: string
  options: string[]
}

const emit = defineEmits<Emits>()
const props = defineProps<Props>()

const isSelecting = ref<boolean>(false)

const model = computed<ModelValue>({
  get() {
    return props.modelValue
  },
  set(value) {
    emit('update:modelValue', value)
  },
})

const select = (option: Props['options'][number]): void => {
  model.value = option
  isSelecting.value = false
}
</script>
