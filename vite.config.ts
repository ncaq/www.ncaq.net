import { glob } from "node:fs/promises";
import { resolve } from "node:path";
import process from "node:process";
import { bundle } from "lightningcss";
import { defineConfig, type Plugin } from "vite";

const outDir = "site/dist" as const;

const cssEntryPoint = "site/style/index.css" as const;

const cssOutFile = "bundle.css" as const;

const dummyCssEntry = "virtual:css-bundle-noop" as const;

/**
 * Vite/rolldownのCSS-onlyエントリポイントではbuild.sourcemapがCSSに反映されない既知の制限がある。
 * https://github.com/vitejs/vite/issues/2830
 *
 * lightningcssのbundle APIだけで`@import`解決, ミニファイ, ソースマップ生成までまかなえるため、
 * Viteには空の仮想エントリだけを渡してCSS処理を一切行わせず、
 * このプラグインだけがCSSバンドルを担当する。
 * 出力先の作成や書き込みはVite側に任せ、生成物は`emitFile`で渡す。
 */
function cssBundleWithSourcemap(): Plugin {
  return {
    name: "css-bundle-with-sourcemap",
    apply: "build",
    resolveId(id) {
      return id === dummyCssEntry ? `\0${dummyCssEntry}` : null;
    },
    load(id) {
      return id === `\0${dummyCssEntry}` ? "" : null;
    },
    async buildStart() {
      // watchモードでCSS変更を検知できるよう、対象のCSSファイルを全て監視対象として登録する。
      for await (const file of glob("site/style/**/*.css")) {
        this.addWatchFile(resolve(file));
      }
      const { code, map } = bundle({
        filename: resolve(cssEntryPoint),
        minify: true,
        sourceMap: true,
        projectRoot: process.cwd(),
      });
      if (map == null) {
        throw new Error("lightningcss did not produce a sourcemap");
      }
      this.emitFile({
        type: "asset",
        fileName: cssOutFile,
        source: `${code.toString()}\n/*# sourceMappingURL=${cssOutFile}.map */`,
      });
      this.emitFile({
        type: "asset",
        fileName: `${cssOutFile}.map`,
        source: map,
      });
    },
    generateBundle(_options, bundleFiles) {
      // 仮想エントリ由来の空JSチャンクは出力する必要がないので破棄する。
      for (const fileName of Object.keys(bundleFiles)) {
        if (fileName !== cssOutFile && fileName !== `${cssOutFile}.map`) {
          delete bundleFiles[fileName];
        }
      }
    },
  };
}

export default defineConfig({
  build: {
    outDir,
    emptyOutDir: true,
    rolldownOptions: {
      input: dummyCssEntry,
    },
  },
  plugins: [cssBundleWithSourcemap()],
});
