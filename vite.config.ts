import { defineConfig } from "vite";

export default defineConfig({
  build: {
    minify: false,
    rolldownOptions: {
      input: "site/style/main.css",
      output: {
        assetFileNames: "dist/bundle.css",
      },
    },
  },
});
