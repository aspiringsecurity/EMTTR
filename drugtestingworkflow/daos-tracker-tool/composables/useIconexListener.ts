import { useUserStore } from '@/stores/user'

enum EVENTS_TYPES {
  RESPONSE_HAS_ACCOUNT = 'RESPONSE_HAS_ACCOUNT',
  RESPONSE_ADDRESS = 'RESPONSE_ADDRESS',
  RESPONSE_JSON_RPC = 'RESPONSE_JSON-RPC',
  RESPONSE_SIGNING = 'RESPONSE_SIGNING',
  CANCEL = 'CANCEL',
  CANCEL_JSON_RPC = 'CANCEL_JSON-RPC',
}

export enum EVENTS_NAMES {
  ICONEX_ACCOUNT = 'ICONEX_ACCOUNT',
  ICONEX_ADDRESS = 'ICONEX_ADDRESS',
  ICONEX_CANCEL = 'ICONEX_CANCEL',
  ICONEX_RPC = 'ICONEX_RPC',
  ICONEX_SIGN = 'ICONEX_SIGN',
}

type EventParams = {
  event: EVENTS_NAMES
}

type AccountParams = {
  payload: Record<string, boolean>
  error?: string
}

type AddressParams = {
  payload: Record<string, string | null>
  error?: string
}

type CancelParams = {
  error?: string
}

type RpcParams = {
  payload?: Record<string, unknown | never>
  error?: string
}

type SignParams = {
  payload: string | null
  error?: string
}

type UnionParams =
  | AccountParams
  | AddressParams
  | CancelParams
  | RpcParams
  | SignParams

type TypedParams<U extends UnionParams> = EventParams & U

type ParserReponse = EventParams & UnionParams

export type EventsParams = {
  [EVENTS_NAMES.ICONEX_ACCOUNT]: AccountParams
  [EVENTS_NAMES.ICONEX_ADDRESS]: AddressParams
  [EVENTS_NAMES.ICONEX_CANCEL]: CancelParams
  [EVENTS_NAMES.ICONEX_RPC]: RpcParams
  [EVENTS_NAMES.ICONEX_SIGN]: SignParams
}

type EventsPayloads = {
  [EVENTS_TYPES.RESPONSE_HAS_ACCOUNT]: { hasAccount: boolean }
  [EVENTS_TYPES.RESPONSE_ADDRESS]: string
  [EVENTS_TYPES.RESPONSE_JSON_RPC]: { code?: string, message?: string }
  [EVENTS_TYPES.RESPONSE_SIGNING]: string
  [EVENTS_TYPES.CANCEL]: never
  [EVENTS_TYPES.CANCEL_JSON_RPC]: never
}

type ValueOf<T> = T[keyof T]

type IconexResponse = Event & {
  detail: ValueOf<{
    [Type in keyof EventsPayloads]: {
      type: Type,
      payload: EventsPayloads[Type]
    }
  }>
}

export const useIconexListener = () => {
  const { loginUser } = useUserStore()
  const { emit, events } = useEventsBus()
  const { notify } = useNotificationToast()

  const parsedEvent = ({ detail }: IconexResponse): ParserReponse => {
    const { type, payload } = detail

    switch (type) {
      case EVENTS_TYPES.RESPONSE_HAS_ACCOUNT:
        return {
          event: events.ICONEX_ACCOUNT,
          payload,
          ...!payload.hasAccount && { error: 'We found no ICONex account. Please install the ICONex Chrome Extension: https://chrome.google.com/webstore/detail/iconex/flpiciilemghbmfalicajoolhkkenfel' },
        } as TypedParams<AccountParams>
      case EVENTS_TYPES.RESPONSE_ADDRESS:
        return {
          event: events.ICONEX_ADDRESS,
          payload: { address: payload || null, wallet: payload ? 'iconex' : null },
          ...!payload && { error: 'We found no ICON wallet. Please create one.' },
        } as TypedParams<AddressParams>
      case EVENTS_TYPES.RESPONSE_JSON_RPC:
        return {
          event: events.ICONEX_RPC,
          payload: payload || {},
          ...(payload?.code && payload?.message) && { error: payload.message },
        } as TypedParams<RpcParams>
      case EVENTS_TYPES.RESPONSE_SIGNING:
        return {
          event: events.ICONEX_SIGN,
          payload: payload || null,
          ...!payload && { error: 'Couldn\'t get signature' },
        } as TypedParams<SignParams>
      case EVENTS_TYPES.CANCEL || EVENTS_TYPES.CANCEL_JSON_RPC:
        return {
          event: events.ICONEX_CANCEL,
          ...type === EVENTS_TYPES.CANCEL_JSON_RPC && { error: 'Transaction cancelled' },
        } as TypedParams<CancelParams>
      default:
        return {
          event: events.ICONEX_CANCEL,
        } as TypedParams<CancelParams>
    }
  }

  const listenIconex = () => {
    window.addEventListener('ICONEX_RELAY_RESPONSE', (response: IconexResponse) => {
      const { event, ...rest } = parsedEvent(response)
      emit(event, rest)
    })
  }

  const ICONEX_HANDLE_ACCOUNT = ({ error, payload }: AccountParams): void => {
    if (error) {
      notify.error({
        title: 'Error',
        message: error,
        timeout: 5000,
      })
    } else if (payload.hasAccount) {
      window.dispatchEvent(new CustomEvent('ICONEX_RELAY_REQUEST', {
        detail: { type: 'REQUEST_ADDRESS' },
      }))
    }
  }

  const ICONEX_HANDLE_ADDRESS = ({ error, payload }: AddressParams): void => {
    if (error) {
      notify.error({
        title: 'Error',
        message: error,
        timeout: 5000,
      })
    } else if (payload.address) {
      loginUser({ address: payload.address, wallet: 'iconex' })
      emit(events.POPUP_CLOSE, { handlePending: true })
      notify.success({
        title: 'Log in successful',
        timeout: 5000,
      })
    }
  }

  const ICONEX_HANDLE_CANCEL = ({ error }: CancelParams): void => {
    if (error) {
      emit(events.POPUP_CLOSE)

      notify.error({
        title: 'Error',
        message: error,
        timeout: 5000,
      })
    }
  }

  return {
    listenIconex,
    ICONEX_HANDLE_ACCOUNT,
    ICONEX_HANDLE_ADDRESS,
    ICONEX_HANDLE_CANCEL,
  }
}
