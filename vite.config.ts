import process from "node:process";
import { bundle } from "lightningcss";
import { defineConfig, type Plugin } from "vite";
import { resolve } from "node:path";
import { writeFile } from "node:fs/promises";

const outDir = "site/dist" as const;

const cssEntryPoint = "site/style/index.css" as const;

const cssOutFile = "bundle.css" as const;

/**
 * ViteのCSS-onlyエントリポイントではbuild.sourcemapがCSSに適用されない既知の制限がある。
 * https://github.com/vitejs/vite/issues/2830
 *
 * lightningcssのbundle APIは`@import`解決, ミニファイ, ソースマップ生成を一気通貫で行えるので、
 * closeBundleでViteの出力を上書きしてバンドル後でも元のCSSをデバッグできるようにする。
 */
function cssBundleWithSourcemap(): Plugin {
  return {
    name: "css-bundle-with-sourcemap",
    apply: "build",
    enforce: "post",
    async closeBundle() {
      const { code, map } = bundle({
        filename: resolve(cssEntryPoint),
        minify: true,
        sourceMap: true,
        projectRoot: process.cwd(),
      });
      if (map == null) {
        throw new Error("lightningcss did not produce a sourcemap");
      }
      const cssOutPath = resolve(outDir, cssOutFile);
      const cssWithRef = `${code.toString()}\n/*# sourceMappingURL=${cssOutFile}.map */`;
      await Promise.all([writeFile(cssOutPath, cssWithRef), writeFile(`${cssOutPath}.map`, map)]);
    },
  };
}

export default defineConfig({
  build: {
    outDir,
    emptyOutDir: true,
    rolldownOptions: {
      input: cssEntryPoint,
      output: {
        assetFileNames: cssOutFile,
      },
    },
  },
  plugins: [cssBundleWithSourcemap()],
});
