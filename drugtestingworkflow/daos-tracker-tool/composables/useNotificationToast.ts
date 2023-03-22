const types = [
  'success',
  'warning',
  'error',
  'info',
] as const

const positions = [
  'top-left',
  'top-right',
  'bottom-left',
  'bottom-right',
] as const

export type NotificationToastTypes = typeof types[number]

export type NotificationToastPositions = typeof positions[number]

type NotificationToastOptionalProps = {
  type?: NotificationToastTypes
  position?: NotificationToastPositions
  width?: number
  timeout?: number
  speed?: number
}

type NotificationToastRequiredProps =
  | { title: string, message?: string }
  | { message: string, title?: string }

type NotificationToastMainHandlerProps = NotificationToastOptionalProps & NotificationToastRequiredProps
type NotificationToastSubHandlersProps = Omit<NotificationToastOptionalProps, 'type'> & NotificationToastRequiredProps

type NotificationToastMainHandlerCallback = { (params: string | NotificationToastMainHandlerProps): string }
type NotificationToastSubHandlersCallbacks = { [key in typeof types[number]]: (params: string | NotificationToastSubHandlersProps) => string }

type NotificationToastSettingsProps = {
  offsetY?: number
  offsetX?: number
  gap?: number
}

type NotificationToastGlobalProps = {
  uid: string
  height?: number
  isActive: boolean
  isVisible: boolean
}

export type NotificationToastProps =
  & NotificationToastMainHandlerProps
  & NotificationToastSettingsProps
  & NotificationToastGlobalProps

export const useNotificationToast = (settings?: NotificationToastSettingsProps) => {
  const notificationsToast = useState<NotificationToastProps[]>('notifications-toast', () => [])

  watch(notificationsToast, (value) => {
    if (value.length && value.map(({ isActive }) => !isActive).every(Boolean)) notificationsToast.value = []
  }, { deep: true })

  const handleNotificationToast: NotificationToastMainHandlerCallback = (params: string | NotificationToastMainHandlerProps): string => {
    const uid: string = Date.now().toString(36) + Math.random().toString(36).split('.')[1]
    notificationsToast.value.push({
      uid,
      isActive: true,
      isVisible: false,
      ...settings,
      ...typeof params === 'string' ? { message: params } : params,
    })

    return uid
  }

  types.forEach((type: NotificationToastTypes) => {
    handleNotificationToast[type] = (params: string | NotificationToastSubHandlersProps): string => handleNotificationToast({
      ...typeof params === 'string' ? { message: params } : params,
      type,
    })
  })

  const notify = handleNotificationToast as NotificationToastMainHandlerCallback & NotificationToastSubHandlersCallbacks

  const close = (...uids: string[]): void => {
    uids.forEach((uid) => {
      const notificationToastIndex: number = notificationsToast.value.indexOf(notificationsToast.value.find((notification) => notification.uid === uid))
      if (notificationToastIndex > -1) notificationsToast.value[notificationToastIndex].isVisible = false
    })
  }

  return {
    notify,
    close,
  }
}
