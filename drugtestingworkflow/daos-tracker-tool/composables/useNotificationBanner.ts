const types = [
  'success',
  'warning',
  'error',
  'info',
] as const

export type NotificationBannerTypes = typeof types[number]

type NotificationBannerOptionalProps = {
  type?: NotificationBannerTypes
  message: string
  speed?: number
}

type NotificationBannerGlobalProps = {
  isActive: boolean
  isVisible: boolean
}

export type NotificationBannerProps = NotificationBannerOptionalProps & NotificationBannerGlobalProps

export const useNotificationBanner = () => {
  const pendingNotificationsBanner = ref<NotificationBannerProps>(null)
  const currentNotificationsBanner = useState<NotificationBannerProps>('notifications-banner', () => null)

  watch(currentNotificationsBanner, async (value) => {
    if (value && !value.isActive) {
      currentNotificationsBanner.value = null

      await nextTick()
      if (pendingNotificationsBanner.value) {
        currentNotificationsBanner.value = pendingNotificationsBanner.value
        pendingNotificationsBanner.value = null
      }
    }
  }, { deep: true })

  const notify = (params: string | NotificationBannerOptionalProps): void => {
    const newNotificationBanner: NotificationBannerProps = {
      ...typeof params === 'string' ? { message: params } : params,
      isActive: true,
      isVisible: false,
    }
    if (currentNotificationsBanner.value) {
      currentNotificationsBanner.value.isVisible = false
      pendingNotificationsBanner.value = newNotificationBanner
    } else {
      currentNotificationsBanner.value = newNotificationBanner
    }
  }

  const close = (): void => {
    if (currentNotificationsBanner.value) currentNotificationsBanner.value.isVisible = false
  }

  return {
    notify,
    close,
  }
}
