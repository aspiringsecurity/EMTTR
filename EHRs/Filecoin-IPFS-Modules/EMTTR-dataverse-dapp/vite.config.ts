import { defineConfig } from "vite";
import reactRefresh from "@vitejs/plugin-react-refresh";

export default defineConfig({
  plugins: [reactRefresh()],
  resolve: {
    dedupe: ["react", "react-dom"],
    alias: {
      web3: "web3/dist/web3.min.js",
      // buffer: "rollup-plugin-node-polyfills/polyfills/buffer-es6", // add buffer
    },
  },
  build: {
    target: "es2020",
    sourcemap: true,
    rollupOptions: {
      external: ["@lit-protocol/sdk-nodejs"],
    },
  },
  server: {
    port: 5173,
    host: "0.0.0.0",
  },
  define: {
    "process.env": {
      ENV: "Browser",
    },
  },
});
