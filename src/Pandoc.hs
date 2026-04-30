module Pandoc (pandocCompilerCustom) where

import Data.Convertible
import qualified Data.Text as T
import Hakyll
import Text.Pandoc
import Text.Pandoc.Transforms (eastAsianLineBreakFilter)

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
         in RawBlock (Format "html") . convert
              <$> unixFilter
                "uv"
                (["run", "pygmentize", "-f", "html"] <> if T.null fileName then [] else ["-l", convert fileName])
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
