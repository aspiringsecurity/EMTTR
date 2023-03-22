import { defineStore } from 'pinia'

type Images = Record<string, string>

export const useImagesStore = defineStore('images-store', () => {
  // States
  const images = reactive<Images>({})

  // Actions
  const setImage = (key: string, url: string): void => {
    images[key] = url
  }

  return {
    // States
    images,

    // Actions
    setImage,
  }
}, { persist: true })
