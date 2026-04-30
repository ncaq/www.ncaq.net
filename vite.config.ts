import { defineConfig } from "vite";

export default defineConfig({
  build: {
    cssCodeSplit: false,
  },
  css: {
    devSourcemap: true,
  },
});
