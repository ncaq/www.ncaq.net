import { glob } from "node:fs/promises";
import { resolve } from "node:path";
import process from "node:process";
import { bundle } from "lightningcss";
import { defineConfig, type Plugin } from "vite";

const outDir = "site/dist" as const;

const cssEntryPoint = "site/style/index.css" as const;

const cssSourceGlob = "site/style/**/*.css" as const;

const cssOutFile = "bundle.css" as const;

const dummyCssEntry = "virtual:css-bundle-noop" as const;

/**
 * lightningcssのvisitor APIで全custom property宣言と`var()`参照を集め、
 * 外部参照を起点に参照グラフを推移閉包で辿って到達不能なものを未使用として返す。
 *
 * `var()`参照には2種類ある:
 * - 通常の宣言値からの参照(ex. `color: var(--m-fg-main)`)は外部参照
 * - custom property値からの参照(ex. `--m-cursor: var(--m-fg-main)`)は内部参照で、
 *   そのproperty自体が使われている時だけ有効
 *
 * Declaration/DeclarationExit visitorで現在のcustom property名をスタックに積めば、
 * Variable visitorは現在地が内側か外側かを判定できる。
 *
 * 結果はlightningcssの`unusedSymbols`にそのまま渡せる`--`付きの名前。
 */
function collectUnusedCustomProperties(): string[] {
  const customPropertyStack: string[] = [];
  const definitions = new Map<string, Set<string>>();
  const externalReferences = new Set<string>();
  bundle({
    filename: resolve(cssEntryPoint),
    projectRoot: process.cwd(),
    visitor: {
      Declaration: {
        custom(property) {
          const name = property.name;
          customPropertyStack.push(name);
          if (!definitions.has(name)) {
            definitions.set(name, new Set<string>());
          }
        },
      },
      DeclarationExit: {
        custom() {
          customPropertyStack.pop();
        },
      },
      Variable(variable) {
        const name = variable.name.ident;
        const current = customPropertyStack.at(-1);
        if (current == null) {
          externalReferences.add(name);
        } else {
          definitions.get(current)?.add(name);
        }
      },
    },
  });
  const reachable = new Set(externalReferences);
  const queue = [...externalReferences];
  while (0 < queue.length) {
    const name = queue.shift();
    if (name == null) {
      continue;
    }
    const refs = definitions.get(name);
    if (refs == null) {
      continue;
    }
    refs.forEach((ref) => {
      if (!reachable.has(ref)) {
        reachable.add(ref);
        queue.push(ref);
      }
    });
  }
  return [...definitions.keys()].filter((name) => !reachable.has(name));
}

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
      for await (const file of glob(cssSourceGlob)) {
        this.addWatchFile(resolve(file));
      }
      const unusedSymbols = collectUnusedCustomProperties();
      const { code, map } = bundle({
        filename: resolve(cssEntryPoint),
        minify: true,
        sourceMap: true,
        projectRoot: process.cwd(),
        unusedSymbols,
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
      Object.keys(bundleFiles)
        .filter((fileName) => fileName !== cssOutFile && fileName !== `${cssOutFile}.map`)
        .forEach((fileName) => delete bundleFiles[fileName]);
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
