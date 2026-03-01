module Main (main) where

import Control.Applicative
import Data.Convertible
import qualified Data.List as L
import qualified Data.List.Split as L
import Data.Maybe
import qualified Data.Text as T
import Hakyll
import System.Directory
import System.FilePath
import Text.Pandoc
import Text.Pandoc.Shared (eastAsianLineBreakFilter)
import Text.Regex.TDFA hiding (empty, match)

main :: IO ()
main =
  getEntryAndYears
    >>= hakyllRun

-- | 年一覧をスマートにページネーションする方法がわからなかったので`IO`でごり押しする。
-- `Rules`は内部的には`IO`をベースに持つが、
-- unsafe系以外で持ち上げる方法が見つからないので、
-- 別コンテキストで処理する。
getEntryAndYears :: IO (String, [String])
getEntryAndYears = do
  entryIndex <- readFile "site/entry-index.html"
  years <- yearInEntry
  return (entryIndex, years)

-- | entryディレクトリ下にある記事が存在する年をリストで返却する。
-- 0埋めされてる可能性も考慮して`Int`ではなくあえて`String`。
-- Hakyllのフレームワークに乗りたかった気持ちはあるが、
-- メタデータではなく`Context`下で取り出す方法がよくわからなかった。
-- 規則性があるので今回はこれで問題ないと判断した。
yearInEntry :: IO [String]
yearInEntry = do
  entryList <- L.sort <$> listDirectory "site/entry"
  let getYear = L.takeWhile (/= '-')
  return $ reverse $ L.nub $ getYear <$> entryList

-- | 下準備が終わった後hakyllを実際に起動する。
hakyllRun :: (String, [String]) -> IO ()
hakyllRun (entryIndex, years) = hakyllWith conf $ do
  match "templates/*" $ compile templateCompiler

  match ("_headers" .||. "*.ico" .||. "*.png" .||. "*.webp" .||. "*.svg" .||. "*.txt" .||. "asset/*") $ do
    route idRoute
    compile copyFileCompiler

  match "default.scss" $ do
    route $ setExtension "css"
    compile $ unixFilter "npm" ["run", "default.css"] "" >>= makeItem

  let entryIndexField = listField "entry-index" defaultContext (pure $ (\x -> Item (fromFilePath x) x) <$> years)

  -- 404はCloudflare Pages的に404/index.htmlではなく404.htmlである必要があるため特別に処理する。
  match "404.md" $ do
    route $ setExtension "html"
    compile $
      pandocCompilerCustom
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/entry.html" entryContext
        >>= loadAndApplyTemplate "templates/default.html" (addTitleWithSuffix <> entryIndexField <> entryContext)
        >>= tidyHtml

  -- 大多数の記事。
  match ("*.md" .||. "entry/*.md") $ do
    route cleanRoute
    compile $
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

-- | Hakyllに渡す設定。
conf :: Configuration
conf =
  def
    { providerDirectory = "site"
    , deployCommand = "npm run deploy"
    }

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
                "uv"
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

-- | Markdownの記事として独立しているページ向け。
entryContext :: Context String
entryContext =
  let context =
        mconcat
          [ titleEscape
          , mconcat entryDate
          , teaserFieldByResource 256 "teaser" "content" id
          , teaserFieldByResource 100 "description" "content" escapeDoubleQuote
          , teaserFieldByResource 180 "og-description" "content" escapeDoubleQuote
          , basicContext
          ]
   in mconcat
        [ context
        , openGraphField "opengraph" context
        ]
 where
  titleEscape = mapTitleEx (fmap escapeHtml)
  entryDate = f <$> ["date", "published"]
   where
    f key =
      field
        key
        ( \item -> do
            mMeta <- getMetadataField (itemIdentifier item) key
            case mMeta of
              Nothing -> return $ fromMaybe empty $ mItemDate item
              Just meta -> return meta
        )
  mItemDate item = case L.splitOneOf "-" f of
    [year, month, day, hour, minute, second] ->
      Just $ concat [year, "-", month, "-", day, "T", hour, ":", minute, ":", second, "+09:00"]
    [year, month, day] ->
      Just $ concat [year, "-", month, "-", day]
    _ -> Nothing
   where
    f = toFilePath $ cleanIdentifier $ itemIdentifier item
    cleanIdentifier = fromFilePath . dropExtension . takeFileName . toFilePath

-- | フィールドのtitleデータを編集します。
-- タイトルが無い場合はエラーを出力して終了します。
mapTitleEx :: (MonadMetadata f) => (f String -> Compiler String) -> Context a
mapTitleEx f = field "title" (f . getTitleEx)

-- | メタデータからtitleを取得します。
-- タイトルが無い場合はエラーを出力して終了します。
getTitleEx :: (MonadMetadata f) => Item a -> f String
getTitleEx item =
  fromMaybe (error "title not found")
    <$> getMetadataField (itemIdentifier item) "title"

-- | Markdownの記事ではなくHTMLから生成する一覧ページ。
indexContext :: String -> String -> String -> Context String -> Context String
indexContext title description ogType entryIndexField =
  mconcat
    [ constField "title" title
    , constField "description" description
    , constField "og-type" ogType
    , constField "og-description" description
    , entryIndexField
    , basicContext
    ]

-- | どのページにも共通する基本的な情報。
basicContext :: Context String
basicContext =
  let context =
        mconcat
          [ constField "root" "https://www.ncaq.net"
          , cleanUrlField
          , constField "og-image" "https://www.ncaq.net/favicon.png"
          , constField "twitter-creator" "@ncaq"
          , constField "twitter-site" "@ncaq"
          , defaultContext
          ]
   in mconcat
        -- JSON-LDやOpenGraphはtypeがハードコーディングされているからあえて入れない。
        [ twitterCardField "twitter" context
        , context
        ]

-- | 記事一覧などに利用するために記事の冒頭を指定された文字数で切り取ってキーに設定する。
teaserFieldByResource :: Int -> String -> Snapshot -> (String -> String) -> Context a
teaserFieldByResource size key snapshot escape = field key $ \item ->
  dropWarningHtmlEntity . take size . escape . stripTags . itemBody
    <$> loadSnapshot (itemIdentifier item) snapshot

-- | HTMLエスケープされた記号が中途半端に残って別の意味になるのを防ぐため、
-- 切り取った箇所の末尾がHTMLエスケープっぽかったら削除する。
-- 実装が非常に雑。
dropWarningHtmlEntity :: String -> String
dropWarningHtmlEntity entity =
  let (safety, _match, _after) = entity =~ ("&[^&;]*$" :: String) :: (String, String, String)
   in safety

-- | ダブルクオートを雑にエスケープする。
-- 雑すぎるのでユーザが入力する可能性のある値を入れる場合許容できないが、
-- 今回は私が自分で入力する値だけが入るので許容する。
escapeDoubleQuote :: String -> String
escapeDoubleQuote = convert . T.replace "\"" "&quot;" . convert

-- | サイトの名前込みのタイトルを設定する。
addTitleWithSuffix :: Context a
addTitleWithSuffix = mapTitleEx (fmap (<> " - ncaq"))

-- | RSS Feed配信向けの設定。
feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "ncaq"
    , feedDescription = "www.ncaq.net entry"
    , feedAuthorName = "ncaq"
    , feedAuthorEmail = "ncaq@ncaq.net"
    , feedRoot = "https://www.ncaq.net"
    }

-- | ファイルパスのハイフンをディレクトリ区切りに変換して、
-- 末尾に`index.html`を付与してディレクトリのデフォルトとして参照されるようにする。
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute `composeRoutes` customHyphenToSlash
 where
  createIndexRoute ident = takeBaseName (dropExtension (toFilePath ident)) </> "index.html"

-- | ファイルパスのハイフンをディレクトリの区切り文字に変換する。
customHyphenToSlash :: Routes
customHyphenToSlash = customRoute (replaceHyphenToSlash . toFilePath)

-- | ハイフンをスラッシュに変換する。
replaceHyphenToSlash :: String -> String
replaceHyphenToSlash = convert . T.replace "-" "/" . convert

-- | urlフィールドにファイルからではない適切なurlを入力する。
cleanUrlField :: Context a
cleanUrlField =
  field
    "url"
    ( fmap (maybe empty $ (replaceHyphenToSlash . cleanUrlString) . toUrl)
        . getRoute
        . itemIdentifier
    )

-- | 末尾のindex.htmlを除去してパスだけで閲覧できるようにする。
cleanUrlString :: String -> String
cleanUrlString = cleanIndex
 where
  cleanIndex path
    | "/index.html" `L.isSuffixOf` path = dropFileName path
    | otherwise = path

-- | HTMLとして正しいかをチェックだけして、
-- 内容は変更せずに返す。
tidyHtml :: Item String -> Compiler (Item String)
tidyHtml item = check item >> pure item
 where
  check =
    withItemBody $
      unixFilter
        "tidy"
        [ "--errors"
        , "--mute-id"
        , "y"
        , "--wrap"
        , "0"
        , "--drop-empty-elements"
        , "n"
        , "--tidy-mark"
        , "n"
        , -- 古いtidyではHTML属性情報が正しくないことがあるので、チェックを諦めます。
          "--warn-proprietary-attributes"
        , "n"
        ]

-- | XMLとして正しいかをチェックだけして、
-- 内容は変更せずに返す。
tidyXml :: Item String -> Compiler (Item String)
tidyXml item = check item >> pure item
 where
  check =
    withItemBody $
      unixFilter
        "tidy"
        [ "--errors"
        , "--mute-id"
        , "y"
        , "--wrap"
        , "0"
        , "-xml"
        ]
