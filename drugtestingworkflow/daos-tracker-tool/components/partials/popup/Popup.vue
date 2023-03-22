<template>
  <div
    class="fixed top-0 left-0 grid w-screen h-screen z-100 py-10"
    @click="closeOnClick"
  >
    <Container class="grid gap-40">
      <div
        class="grid gap-16 content-start justify-self-center self-center w-full p-20 bg-grey-300 rounded-20 transition-width duration-400"
        :class="{
          's:w-256': size === 's',
          's:w-512': size === 'm',
          's:w-1024': size === 'l',
        }"
        @click.stop
      >
        <div class="grid gap-10 grid-cols-1fr-auto items-center">
          <h3 class="typo-title-s">
            <slot name="header" />
          </h3>
          <button
            v-if="requireButton"
            class="grid place-items-center w-20 h-20 text-white hover:text-primary"
            @click="closeOnClick"
          >
            <UtilsIcon
              name="Cross"
              class="w-20 h-20"
            />
          </button>
        </div>
        <slot name="body" />
      </div>
    </Container>
  </div>
</template>

<script setup lang="ts">
type Size = 's' | 'm' | 'l'

type Props = {
  blockOverlayClick?: boolean
  requireButton?: boolean
  size?: Size
}

withDefaults(defineProps<Props>(), {
  blockOverlayClick: false,
  requireButton: false,
  size: 'm',
})

const { emit, events } = useEventsBus()

const closeOnClick = (): void => {
  emit(events.POPUP_CLOSE)
}

const closeOnEscape = ({ key }: KeyboardEvent): void => {
  if (key === 'Escape') {
    closeOnClick()
  }
}

onMounted(() => {
  window.addEventListener('keydown', closeOnEscape)
})

onUnmounted(() => {
  window.removeEventListener('keydown', closeOnEscape)
})
</script>
