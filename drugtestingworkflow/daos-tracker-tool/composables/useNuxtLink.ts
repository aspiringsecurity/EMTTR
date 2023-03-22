import type { NuxtLinkOptions } from '#app'

export const useNuxtLink = (options?: NuxtLinkOptions) => {
  const NuxtLink = defineNuxtLink(options || { componentName: 'NuxtLink' })

  return {
    NuxtLink,
  }
}
