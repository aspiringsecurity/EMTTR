import { EVENTS_NAMES as ICONEX_LISTENER_EVENTS_NAMES, EventsParams as IconexListenerEventsParams } from './useIconexListener'
import { EVENTS_NAMES as POPUP_METHODS_EVENTS_NAMES, EventsParams as PopupMethodsEventsParams } from './usePopupMethods'

type EventsNames =
  | ICONEX_LISTENER_EVENTS_NAMES
  | POPUP_METHODS_EVENTS_NAMES

type EventsParams =
  & IconexListenerEventsParams
  & PopupMethodsEventsParams

const bus = ref<Map<EventsNames, undefined | EventsParams[EventsNames]>>(new Map())

export const useEventsBus = () => ({
  bus,
  emit: <E extends EventsNames>(
    event: E,
    ...params: Partial<EventsParams[E]> extends EventsParams[E] ? [(undefined | EventsParams[E])?] : [EventsParams[E]]
  ): void => {
    bus.value.set(event, params[0] ?? {})
  },
  events: {
    ...ICONEX_LISTENER_EVENTS_NAMES,
    ...POPUP_METHODS_EVENTS_NAMES,
  } as const,
})
