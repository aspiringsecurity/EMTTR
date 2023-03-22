type PopupStructure = {
  component?: object | string | null
  params?: object
  events?: object
  callback?: ((...args: unknown[]) => void) | null
  onClose?: ((...args: unknown[]) => void) | null
}

const basePopupData: PopupStructure = {
  component: null,
  params: {},
  events: {},
  callback: null,
} as const

Object.freeze(basePopupData)

export const usePopupData = (): [PopupStructure, (newPopupData?: PopupStructure) => void] => {
  const popupData = reactive<PopupStructure>({ ...basePopupData })

  const setPopupData = (newPopupData: PopupStructure = basePopupData): void => {
    Object.entries(newPopupData).forEach(([key, value]: [string, unknown]) => {
      popupData[key] = value
    })
  }

  return [popupData, setPopupData]
}
