import { defineStore } from 'pinia'
import { truncate } from '@/assets/scripts/helpers'

export const useUserStore = defineStore('user-store', () => {
  // States
  const isLoggedIn = ref<boolean>(false)
  const address = ref<string>('')
  const wallet = ref<string>('')

  type UserContracts = {
    token: string
    agora: string
  }

  const userContracts = reactive<UserContracts>({
    token: '',
    agora: '',
  })

  // Getters
  const truncatedAddress = computed<string>(() => (address.value ? truncate(address.value) : ''))

  // Actions
  const loginUser = (params: { address?: string, wallet?: string }): void => {
    isLoggedIn.value = true
    address.value = params.address
    wallet.value = params.wallet
  }
  const logoutUser = (): void => {
    if (wallet.value) {
      window.dispatchEvent(new CustomEvent('bri.widget', {
        detail: {
          action: 'logout',
        },
      }))
    }
    isLoggedIn.value = false
    address.value = ''
    wallet.value = ''
  }
  const setUserContracts = async (key: string, value:string): Promise<void> => {
    userContracts[key] = value
  }

  return {
    // States
    isLoggedIn,
    address,
    wallet,
    userContracts,
    // Getters
    truncatedAddress,

    // Actions
    loginUser,
    logoutUser,
    setUserContracts,
  }
}, { persist: true })
