import path from 'path'
import { defineConfig } from 'vitest/config'

export default defineConfig({
  resolve: {
    alias: {
      '@': path.resolve(__dirname, './src'),
    },
  },
  test: {
    hookTimeout: 20_000,
    testTimeout: 20_000,
    setupFiles: ['./src/testUtils/setup.ts'],
  },
})
