export const useCopyText = (timeout = 1000) => {
  const copyTimeout = ref<NodeJS.Timeout>(null)

  const messageText = ref<string>('')

  const isCopying = computed<boolean>(() => !!messageText.value)

  const copyText = (text: string, message?: string): void => {
    if (navigator) {
      navigator.clipboard.writeText(text)
      clearTimeout(copyTimeout.value)
      messageText.value = message || text

      copyTimeout.value = setTimeout(() => {
        messageText.value = ''
      }, timeout)
    }
  }

  return {
    messageText,
    isCopying,
    copyText,
  }
}
