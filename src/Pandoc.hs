module Pandoc (pandocCompilerCustom) where

import Data.Text qualified as T
import Hakyll
import Himari
import Text.Pandoc
import Text.Pandoc.Transforms (eastAsianLineBreakFilter)

-- | Pandocの設定。
pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom =
  let extensions =
        -- 大したこと無いように見えて結構パフォーマンスに影響するので無効化する。
        disableExtension Ext_pandoc_title_block
          -- 記号を変に変えられるのは困るので無効化する。
          . disableExtension Ext_smart
          -- 一応自動見出し向けのidを入れる。
          . enableExtension Ext_auto_identifiers
          $ readerExtensions defaultHakyllReaderOptions
      -- pygmentizeでシンタックスハイライト。
      transformBlock (CodeBlock (_identifier, classes, _keyValue) str) =
        let fileName = T.unwords classes
         in RawBlock (Format "html") . convert
              <$> unixFilter
                "uv"
                ( ["run", "pygmentize", "-f", "html"]
                    <> if T.null fileName then [] else ["-l", convert fileName]
                )
                (convert str)
      transformBlock x = pure x
   in pandocCompilerWithTransformM
        defaultHakyllReaderOptions
          { readerExtensions = extensions
          }
        ( setMathML . disableHighlight $
            defaultHakyllWriterOptions
              { writerSectionDivs = True -- HTML sectionの方を使う。
              , writerExtensions = extensions
              }
        )
        -- 東アジアの文字列に余計な空白が入らないようにする。
        -- 何故かコマンドラインオプションでは有効にならない。
        (bottomUpM transformBlock . eastAsianLineBreakFilter)

-- nixpkgsとHackageの最新でpandocのバージョンが異なるため、
-- WriterOptionsのフィールド名の変更をCPPで吸収する。

-- | 数式をMathMLで出力する。
setMathML :: WriterOptions -> WriterOptions
#if MIN_VERSION_pandoc(3, 11, 0)
setMathML opts = opts{writerMathMethod = MathML}
#else
setMathML opts = opts{writerHTMLMathMethod = MathML}
#endif

-- | Pygmentを使うのでPandocのハイライトは無効化する。
disableHighlight :: WriterOptions -> WriterOptions
#if MIN_VERSION_pandoc(3, 8, 0)
disableHighlight opts = opts{writerHighlightMethod = NoHighlighting}
#else
disableHighlight opts = opts{writerHighlightStyle = Nothing}
#endif
