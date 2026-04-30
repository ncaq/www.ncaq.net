import { defineConfig } from "vite";

export default defineConfig({
  build: {
    rolldownOptions: {
      input: "site/style/main.css",
      output: {
        assetFileNames: "dist/css/bundle.css",
      },
    },
    cssCodeSplit: false,
  },
});
