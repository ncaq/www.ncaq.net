import { defineConfig } from "vite";

export default defineConfig({
  build: {
    minify: false,
    outDir: "site/dist",
    emptyOutDir: true,
    rolldownOptions: {
      input: "site/style/entry-point.css",
      output: {
        assetFileNames: "bundle.css",
      },
    },
  },
});
