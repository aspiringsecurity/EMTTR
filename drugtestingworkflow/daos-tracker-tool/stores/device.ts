import { defineStore } from 'pinia'

type DeviceState = {
  isMobile?: boolean
  isMobileOrTablet?: boolean
  isDesktopOrTablet?: boolean
  isDesktop?: boolean
  isCrawler?: boolean
  isIos?: boolean
  isAndroid?: boolean
  isWindows?: boolean
  isMacOS?: boolean
}

export const useDeviceStore = defineStore('device-store', () => {
  // States
  const device = reactive<DeviceState>({
    isMobile: false,
    isMobileOrTablet: false,
    isDesktopOrTablet: false,
    isDesktop: false,
    isIos: false,
    isAndroid: false,
    isWindows: false,
    isMacOS: false,
  })
  const browser = ref<string>('')

  // Getters
  const isChrome = computed<boolean>(() => browser.value === 'chrome')

  // Actions
  const setBrowser = (name: string): void => {
    browser.value = name
  }
  const setDevice = (checks: DeviceState): void => {
    Object.entries(checks).forEach(([key, value]: [string, boolean]) => {
      device[key] = value
    })
  }

  return {
    // States
    browser,
    device,

    // Getters
    isChrome,

    // Actions
    setBrowser,
    setDevice,
  }
})
