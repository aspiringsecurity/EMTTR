import { defineNuxtPlugin, useCookie } from '#app'
import { createNuxtPersistedState } from 'pinia-plugin-persistedstate'

export default defineNuxtPlugin((nuxtApp) => {
  nuxtApp.$pinia.use(createNuxtPersistedState(useCookie))
})
