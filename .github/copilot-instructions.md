# For LLM Instructions

## 出力設定

日本語で応答してください。
しかしコードのコメントなどは元の言語のままにしてください。

全角記号より半角記号を優先して使ってください。
特に全角括弧は禁止。

## ディレクトリ構成

### プロンプト

`CLAUDE.md`は`.github/copilot-instructions.md`のシンボリックリンクです。

```
CLAUDE.md -> .github/copilot-instructions.md
```

## Markdown

### Pandoc

Markdownの処理系にはPandocを使っています。

#### 設定

Pandocの標準的拡張に加えて以下のように拡張を有効化しています。

```haskell
-- | Pandocの設定。
pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom =
  let extensions =
        -- 大したこと無いように見えて結構パフォーマンスに影響するので無効化する。
        disableExtension Ext_pandoc_title_block $
          -- 記号を変に変えられるのは困るので無効化する。
          disableExtension Ext_smart $
            -- 一応自動見出し向けのidを入れる。
            enableExtension Ext_auto_identifiers $
              readerExtensions defaultHakyllReaderOptions
      -- pygmentizeでシンタックスハイライト。
      transform (CodeBlock (_identifier, classes, _keyValue) str) =
        let fileName = T.unwords classes
            fileKind = if T.null fileName then T.unwords classes else fileName
         in RawBlock (Format "html") . convert
              <$> unixFilter
                "poetry"
                (["run", "pygmentize", "-f", "html"] <> if T.null fileKind then [] else ["-l", convert fileKind])
                (convert str)
      transform x = return x
   in pandocCompilerWithTransformM
        defaultHakyllReaderOptions
          { readerExtensions = extensions
          }
        defaultHakyllWriterOptions
          { writerHTMLMathMethod = MathML
          , writerSectionDivs = True -- HTML sectionの方を使う。
          , writerExtensions = extensions
          , writerHighlightStyle = Nothing -- 対応言語が多いPygmentでシンタックスハイライトを行うためPandoc側では不要。
          }
        -- 東アジアの文字列に余計な空白が入らないようにする。
        -- 何故かコマンドラインオプションでは有効にならない。
        (bottomUpM transform . eastAsianLineBreakFilter)
```
