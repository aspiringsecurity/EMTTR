const icons = {
  'Arrow/Down': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Arrow/Down.vue'))),
  'Arrow/Left': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Arrow/Left.vue'))),
  'Arrow/Right': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Arrow/Right.vue'))),
  'Arrow/Up': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Arrow/Up.vue'))),
  'Chevron/Down': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Chevron/Down.vue'))),
  'Chevron/Left': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Chevron/Left.vue'))),
  'Chevron/Right': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Chevron/Right.vue'))),
  'Chevron/Up': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Chevron/Up.vue'))),
  'Logo/Discord': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Logo/Discord.vue'))),
  'Logo/Github': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Logo/Github.vue'))),
  'Logo/Icon': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Logo/Icon.vue'))),
  'Logo/Telegram': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Logo/Telegram.vue'))),
  'Logo/Twitter': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Logo/Twitter.vue'))),
  'Logo/Website': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Logo/Website.vue'))),
  'Math/Minus': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Math/Minus.vue'))),
  'Math/Plus': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Math/Plus.vue'))),
  'State/Error': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/State/Error.vue'))),
  'State/Info': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/State/Info.vue'))),
  'State/Success': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/State/Success.vue'))),
  'State/Warning': markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/State/Warning.vue'))),
  Copy: markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Copy.vue'))),
  Cross: markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Cross.vue'))),
  Login: markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Login.vue'))),
  Logout: markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Logout.vue'))),
  Menu: markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Menu.vue'))),
  Message: markRaw(defineAsyncComponent(() => import('@/assets/scripts/icons/Message.vue'))),
}

export type IconsNames = keyof typeof icons
export type IconsComponents = typeof icons[IconsNames]

export const useIconsComponents = (): Record<IconsNames, IconsComponents> => icons
