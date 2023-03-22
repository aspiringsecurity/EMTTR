import { useUserStore } from '@/stores/user'

const popupsComponents = {
  action: {
    DeployToken: markRaw(defineAsyncComponent(() => import('@/components/partials/popup/action/DeployToken.vue'))),
    SetAgora: markRaw(defineAsyncComponent(() => import('@/components/partials/popup/action/SetAgora.vue'))),
  },
  guard: {
    Connect: markRaw(defineAsyncComponent(() => import('@/components/partials/popup/guard/Connect.vue'))),
    Disconnect: markRaw(defineAsyncComponent(() => import('@/components/partials/popup/guard/Disconnect.vue'))),
  },
}

export enum EVENTS_NAMES {
  POPUP_CLOSE = 'POPUP_CLOSE_CURRENT',
  POPUP_GUARD = 'POPUP_HANDLE_GUARD',
  POPUP_ACTION = 'POPUP_CALL_ACTION',
}

type CloseParams = {
  returnData?: unknown
  handlePending?: boolean
}

type GuardParams = {
  callback?: ((...args: unknown[]) => void) | null
  onClose?: ((...args: unknown[]) => void) | null
}

type ActionParams = {
  name: keyof typeof popupsComponents.action
  params?: object
  events?: object
  callback?: ((...args: unknown[]) => void) | null
  onClose?: ((...args: unknown[]) => void) | null
  handleGuard?: boolean
}

export type EventsParams = {
  [EVENTS_NAMES.POPUP_CLOSE]: CloseParams
  [EVENTS_NAMES.POPUP_GUARD]: GuardParams
  [EVENTS_NAMES.POPUP_ACTION]: ActionParams
}

export const usePopupMethods = () => {
  const [currentPopup, setCurrentPopup] = usePopupData()
  const [pendingPopup, setPendingPopup] = usePopupData()

  const POPUP_CLOSE_CURRENT = ({ handlePending = false, returnData }: CloseParams = {}): void => {
    if (!pendingPopup.component || !handlePending) {
      setCurrentPopup()
      if (currentPopup.onClose) {
        currentPopup.onClose(returnData)
      }
    } else {
      const { callback, ...rest } = pendingPopup
      setCurrentPopup({ ...rest })
      setPendingPopup()
      if (callback) {
        callback(rest.params)
      }
    }
  }

  const POPUP_HANDLE_GUARD = ({ callback = null }: GuardParams = {}): void => {
    const { isLoggedIn } = useUserStore()
    setCurrentPopup({ component: isLoggedIn ? popupsComponents.guard.Disconnect : popupsComponents.guard.Connect })

    if (callback) {
      callback()
    }
  }

  const POPUP_CALL_ACTION = ({
    name,
    params = {},
    events = {},
    callback = null,
    onClose = null,
    handleGuard = false,
  }: ActionParams): void => {
    const { isLoggedIn } = useUserStore()
    const popup = { component: popupsComponents.action[name], params, events, onClose }

    if (handleGuard && !isLoggedIn) {
      setPendingPopup({ ...popup, callback })
      POPUP_HANDLE_GUARD()
    } else {
      setCurrentPopup({ ...popup })
      setPendingPopup()
      if (callback) {
        callback(params)
      }
    }
  }

  return {
    currentPopup,
    POPUP_CLOSE_CURRENT,
    POPUP_HANDLE_GUARD,
    POPUP_CALL_ACTION,
  }
}
