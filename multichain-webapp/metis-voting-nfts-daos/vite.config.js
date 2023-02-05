import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import svgr from "@honkhonk/vite-plugin-svgr";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [svgr(), react()],
  // define: {
  //   global: {},
  //   "process.env": {},
  // },
  // optimizeDeps: {
  //   // exclude: ["@coinbase/wallet-sdk"],
  //   // include: ["esm-dep > cjs-dep"],
  // },
  build: {
    minify: false,
  },
});
