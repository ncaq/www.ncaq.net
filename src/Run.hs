module Run (hakyllRun) where

import Configuration
import qualified Data.List as L
import EntryContext
import Feed
import Hakyll
import IndexContext
import Metadata
import Pandoc
import Route
import Tidy
import Title

-- | 下準備が終わった後hakyllを実際に起動する。
hakyllRun :: Options -> (String, [String]) -> IO ()
hakyllRun options (entryIndex, years) = hakyllWithArgs conf options $ do
  match "templates/*" $ compile templateCompiler

  -- 現在トップレベルから自動転写する画像の拡張子一覧。
  let imageExtension :: Pattern =
        foldr
          ((.||.) . fromGlob)
          ""
          [ "*.avif"
          , "*.bmp"
          , "*.gif"
          , "*.ico"
          , "*.jpeg"
          , "*.jpg"
          , "*.png"
          , "*.svg"
          , "*.tif"
          , "*.tiff"
          , "*.webp"
          ]

  match (imageExtension .||. "*.txt" .||. "_headers" .||. "asset/**/*") $ do
    route idRoute
    compile copyFileCompiler

  -- Viteによってバンドルされたスタイルシート。
  -- 実体はmain関数側でHakyll起動前にViteを呼び出して生成しています。
  -- バンドル後もブラウザの開発者ツールから元のCSSを参照できるようにソースマップも転写します。
  match ("dist/bundle.css" .||. "dist/bundle.css.map") $ do
    route idRoute
    compile copyFileCompiler

  let entryIndexField = listField "entry-index" defaultContext (pure $ (\x -> Item (fromFilePath x) x) <$> years)

  -- 404はCloudflare Pages的に404/index.htmlではなく404.htmlである必要があるため特別に処理する。
  match "404.md" $ do
    route $ setExtension "html"
    compile $ do
      identifier <- getUnderlying
      validateMetadata identifier
      pandocCompilerCustom
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/entry.html" entryContext
        >>= loadAndApplyTemplate "templates/default.html" (addTitleWithSuffix <> entryIndexField <> entryContext)
        >>= tidyHtml

  -- 大多数の記事。
  match ("*.md" .||. "entry/*.md") $ do
    route cleanRoute
    compile $ do
      identifier <- getUnderlying
      validateMetadata identifier
      pandocCompilerCustom
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/entry.html" entryContext
        >>= loadAndApplyTemplate "templates/default.html" (addTitleWithSuffix <> entryIndexField <> entryContext)
        >>= tidyHtml

  -- サイトのトップレベル。
  match "index.html" $ do
    route idRoute
    let context =
          listField "entry" entryContext (L.take 5 . reverse <$> loadAll (fromGlob "entry/*.md"))
            <> indexContext
              "ncaq"
              "ncaq website root"
              "website"
              entryIndexField
    compile $
      getResourceBody
        >>= applyAsTemplate context
        >>= loadAndApplyTemplate "templates/default.html" context
        >>= tidyHtml

  -- 年ごとの記事一覧。
  -- 何でも良いけれど適当に区切らないとGoogleがインデックスを拒否する。
  let entryIndexOfYear year = do
        create [fromFilePath $ year <> "/index.html"] $ do
          route $ constRoute $ year <> "/index.html"
          let context =
                listField "entry" entryContext (reverse <$> loadAll (fromGlob $ "entry/" <> year <> "*.md"))
                  <> indexContext
                    (year <> "年の記事一覧 - ncaq")
                    (year <> "年の記事一覧 - ncaq")
                    "website"
                    entryIndexField
          compile $
            makeItem entryIndex
              >>= applyAsTemplate context
              >>= loadAndApplyTemplate "templates/default.html" context
  mapM_ entryIndexOfYear years

  -- ファイル以外の情報を元にサイトマップを毎回更新する。
  create ["sitemap.xml"] $ do
    route idRoute
    let sitemapContext =
          listField "entry" entryContext (reverse . filter not404 <$> loadAll ("*.md" .||. "entry/*.md"))
            <> entryIndexField
        not404 item = toFilePath (itemIdentifier item) /= "404.md"
    compile $
      getResourceBody
        >>= applyAsTemplate sitemapContext
        >>= tidyXml

  -- ファイル以外の情報を元にフィードを毎回更新する。
  -- フィード対象はentry以下のみ。
  create ["feed.atom"] $ do
    route idRoute
    compile $ do
      let feedContext = bodyField "description" <> entryContext
      entry <- take 20 . reverse <$> loadAllSnapshots "entry/*.md" "content"
      renderAtom feedConfiguration feedContext entry >>= tidyXml
