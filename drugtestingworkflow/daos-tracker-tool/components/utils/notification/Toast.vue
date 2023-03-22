<template>
  <div
    ref="root"
    :class="[
      $style.container,
      {
        'pb-20': message,
      }
    ]"
    class="grid gap-10 grid-cols-auto-1fr p-10 bg-grey-300 rounded-5 shadow after:text-primary"
    @mouseenter="isHovering = true"
    @mouseleave="isHovering = false"
  >
    <button
      class="grid place-items-center w-20 h-20 hover:text-primary"
      @click="closeNotificationToast"
    >
      <UtilsIcon
        name="Cross"
        class="w-20 h-20"
      />
    </button>
    <div class="grid gap-10">
      <h3
        v-if="title"
        class="typo-title-s"
      >
        {{ title }}
      </h3>
      <p
        v-if="message"
        class="typo-paragraph"
      >
        {{ message }}
      </p>
    </div>
  </div>
</template>

<script setup lang="ts">
import type { IconsNames } from '@/composables/useIconsComponents'
import type { NotificationToastTypes, NotificationToastPositions, NotificationToastProps } from '@/composables/useNotificationToast'

// TODO: use `NotificationToastProps` type when Vue handles the conditional props (https://github.com/vuejs/core/issues/4294)
type Props = {
  uid: string
  isActive: boolean
  isVisible: boolean
  type?: NotificationToastTypes
  title?: string
  message?: string
  position?: NotificationToastPositions
  width?: number
  height?: number
  timeout?: number
  speed?: number
  offsetY?: number
  offsetX?: number
  gap?: number
}

const props = withDefaults(defineProps<Props>(), {
  type: null,
  title: null,
  message: null,
  position: 'top-right',
  width: 350,
  height: null,
  timeout: 0,
  speed: 250,
  offsetY: 60,
  offsetX: 20,
  gap: 10,
})

const notificationsToast = useState<NotificationToastProps[]>('notifications-toast')

const root = ref<HTMLElement | null>(null)
const isHovering = ref<boolean>(false)
const startDate = ref<number>(0)
const pauseDate = ref<number>(0)
const deltaTimeout = ref<number>(1)
const notificationToastIndex = ref<number>(notificationsToast.value.indexOf(notificationsToast.value.find(({ uid }) => uid === props.uid)))
const currentNotificationToast = ref<NotificationToastProps>(notificationsToast.value[notificationToastIndex.value])

const [posY, posX]: string[] = props.position.split('-')
const offsets = reactive<{
  top: string
  right: string
  bottom: string
  left: string
}>({
  top: posY === 'top' ? `${props.offsetY}px` : 'unset',
  bottom: posY === 'bottom' ? `${props.offsetY}px` : 'unset',
  right: posX === 'right' ? `${props.offsetX}px` : 'unset',
  left: posX === 'left' ? `${props.offsetX}px` : 'unset',
})

const displayTimer = computed<string>(() => (props.timeout ? 'block' : 'none'))

const notificationIcon = computed<IconsNames>(() => {
  switch (props.type) {
    case 'success':
      return 'State/Success'
    case 'warning':
      return 'State/Warning'
    case 'error':
      return 'State/Error'
    case 'info':
      return 'State/Info'
    default:
      return 'Message'
  }
})

const transitionDuration = computed<number>(() => Math.max(1, props.speed))

const translateOffsetX = computed<string>(() => {
  if (props.isVisible && props.isActive) return '0'

  switch (posX) {
    case 'right':
      return `calc(100% + ${offsets.right})`
    case 'left':
      return `calc(-100% - ${offsets.left})`
    default:
      return '0'
  }
})

const translateOffsetY = computed<string>(() => {
  const notificationsToastPositionGroup = notificationsToast.value.filter(({ position, isActive }) => isActive && (position === props.position || (!position && props.position === 'top-right')))
  const notificationToastIndexInGroup = notificationsToastPositionGroup.indexOf(notificationsToastPositionGroup.find(({ uid }) => uid === props.uid))
  const notificationsToastElderGroup = notificationsToastPositionGroup.filter((_, index) => index < notificationToastIndexInGroup)
  const notificationToastOffset = notificationsToastElderGroup.reduce((accu, { height = 0, gap = props.gap }) => accu + height + gap, 0)

  switch (posY) {
    case 'top':
      return `${notificationToastOffset}px`
    case 'bottom':
      return `${-notificationToastOffset}px`
    default:
      return '0'
  }
})

const closeNotificationToast = (): void => {
  currentNotificationToast.value.isVisible = false
}

const closeTimeout = (): void => {
  const currentDate = Date.now()
  deltaTimeout.value = Math.min(1, Math.max(0, 1 - (currentDate - startDate.value) / props.timeout))

  if (currentDate - startDate.value >= props.timeout) {
    closeNotificationToast()
  } else if (currentNotificationToast.value.isVisible && !isHovering.value) {
    requestAnimationFrame(closeTimeout)
  }
}

if (props.timeout) {
  watch(isHovering, (value) => {
    if (value) {
      pauseDate.value = Date.now()
    } else {
      startDate.value = Date.now() - (pauseDate.value - startDate.value)
      closeTimeout()
    }
  })
}

watch(currentNotificationToast, ({ isVisible }) => {
  if (!isVisible) {
    setTimeout(() => {
      currentNotificationToast.value.isActive = false
    }, transitionDuration.value)
  }
}, { deep: true })

onMounted(() => {
  currentNotificationToast.value.height = root.value?.offsetHeight

  if (props.title || props.message) {
    currentNotificationToast.value.isVisible = true

    if (props.timeout) {
      startDate.value = Date.now()
      closeTimeout()
    }
  } else {
    closeNotificationToast()
  }
})
</script>

<style module lang="scss">
.container {
  position: fixed;
  top: v-bind("offsets.top");
  right: v-bind("offsets.right");
  bottom: v-bind("offsets.bottom");
  left: v-bind("offsets.left");
  z-index: 999999;
  width: calc(v-bind(width) * 1px);
  overflow: hidden;
  transform: translateY(v-bind(translateOffsetY)) translateX(v-bind(translateOffsetX));
  transition-timing-function: cubic-bezier(.25, .1, .25, 1);
  transition-duration: calc(v-bind(transitionDuration) * 1ms);
  transition-property: transform;
  will-change: transform;

  &::after {
    position: absolute;
    right: 0;
    bottom: 0;
    left: 0;
    display: v-bind(displayTimer);
    width: 100%;
    height: 4px;
    background: currentColor;
    transform: scaleX(v-bind(deltaTimeout));
    transform-origin: left;
    content: "";
    will-change: transform;
  }
}
</style>
