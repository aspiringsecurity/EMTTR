<template>
  <div
    :class="[
      $style.banner,
      {
        'text-white bg-white': !type,
        'text-success bg-success': type === 'success',
        'text-warning bg-warning': type === 'warning',
        'text-error bg-error': type === 'error',
        'text-info bg-info': type === 'info',
      }
    ]"
    class="grid items-center bg-opacity-15"
  >
    <Container class="grid grid-flow-col justify-center items-center gap-6 overflow-hidden typo-text-regular">
      {{ message }}
      <button
        class="grid place-items-center w-20 h-20 opacity-50 hover:opacity-100"
        @click="closeNotificationBanner"
      >
        <UtilsIcon
          name="Cross"
          class="w-20 h-20"
        />
      </button>
    </Container>
  </div>
</template>

<script setup lang="ts">
import type { NotificationBannerTypes, NotificationBannerProps } from '@/composables/useNotificationBanner'

type Props = {
  type?: NotificationBannerTypes
  message: string
  speed?: number
  isVisible: boolean
}

const props = withDefaults(defineProps<Props>(), {
  type: null,
  speed: 250,
})

const currentNotificationsBanner = useState<NotificationBannerProps>('notifications-banner')

const transitionDuration = computed<number>(() => Math.max(1, props.speed))

const notificationBannerHeight = computed<number>(() => (currentNotificationsBanner.value.isVisible ? 40 : 0))

const closeNotificationBanner = (): void => {
  currentNotificationsBanner.value.isVisible = false
}

watch(currentNotificationsBanner, (newNotificationBanner) => {
  if (newNotificationBanner && !newNotificationBanner.isVisible) {
    setTimeout(() => {
      currentNotificationsBanner.value.isActive = false
    }, transitionDuration.value)
  }
}, { deep: true })

onMounted(() => {
  requestAnimationFrame(() => {
    currentNotificationsBanner.value.isVisible = true
  })
})
</script>

<style lang="scss" module>
.banner {
  height: calc(v-bind(notificationBannerHeight) * 1px);
  transition-timing-function: cubic-bezier(.25, .1, .25, 1);
  transition-duration: calc(v-bind(transitionDuration) * 1ms);
  transition-property: height;
  will-change: transform;
}
</style>
