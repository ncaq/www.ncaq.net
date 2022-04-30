{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.List             (isSuffixOf)
import           Data.List.Split
import           Data.Maybe
import           Data.Convertible
import qualified Data.Text             as T
import           Hakyll
import           System.FilePath
import           Text.Pandoc
import           Text.Pandoc.Shared    (eastAsianLineBreakFilter)
import qualified Text.Regex            as R

main :: IO ()
main = hakyllWith conf $ do
  match "templates/*" $ compile templateCompiler

  match ("*.ico" .||. "*.png" .||. "*.svg" .||. "*.txt" .||. "asset/*") $ do
    route idRoute
    compile copyFileCompiler

  match "default.scss" $ do
    route $ setExtension "css"
    compile $ unixFilter "yarn" ["run", "-s", "default.css"] "" >>= makeItem

  match ("*.md" .||. "entry/*.md") $ do
    route cleanRoute
    compile $ pandocCompilerCustom >>=
      saveSnapshot "content" >>=
      loadAndApplyTemplate "templates/entry.html" entryContext >>=
      loadAndApplyTemplate "templates/default.html" (addTitleSuffix <> entryContext) >>=
      tidyHtml

  match "index.html" $ do
    route idRoute
    let indexContext = listField "entry" entryContext (reverse <$> loadAll "entry/*.md") <>
          constField "title" "ncaq" <>
          constField "type" "website" <>
          constField "og-description" "ncaq website index" <>
          cleanUrlField <>
          defaultContext
    compile $ getResourceBody >>=
      applyAsTemplate indexContext >>=
      loadAndApplyTemplate "templates/default.html" indexContext >>=
      tidyHtml

  create ["sitemap.xml"] $ do
    route idRoute
    let sitemapContext =
          listField "entry" entryContext (reverse . filter not404 <$> loadAll ("*.md" .||. "entry/*.md"))
        not404 item = toFilePath (itemIdentifier item) /= "404.md"
    compile $ getResourceBody >>=
      applyAsTemplate sitemapContext >>=
      tidyXml

  create ["feed.atom"] $ do
    route idRoute
    compile $ do
      let feedContext = entryContext <> bodyField "description"
      entry <- take 20 . reverse <$> loadAllSnapshots "entry/*.md" "content"
      renderAtom feedConfiguration feedContext entry >>=
        tidyXml

conf :: Configuration
conf = def
  { deployCommand =
    unwords
    [ "rsync"
    , "--info=COPY,DEL,FLIST,MISC,NAME,STATS,SYMSAFE,REMOVE"
    , "--checksum"
    , "--archive"
    , "--chmod=D755,F644"
    , "--delete"
    , "--compress"
    , "--human-readable"
    , "--progress"
    , "_site/"
    , "ncaq@ncaq.net:/var/www/www.ncaq.net"
    ]
  }

pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom =
  let extensions =
        -- 大したこと無いように見えて結構パフォーマンスに影響するので無効化します
        disableExtension Ext_pandoc_title_block $
        -- 記号を変に変えられるのは困るので無効化します
        disableExtension Ext_smart $
        -- 一応自動見出しを入れます
        enableExtension Ext_auto_identifiers $
        readerExtensions defaultHakyllReaderOptions
      transform (CodeBlock (_identifier, classes, _keyValue) str) =
        let fileName = T.unwords classes
            fileKind = if T.null fileName then T.unwords classes else fileName
        in RawBlock (Format "html") . convert <$>
           unixFilter "poetry"
           (["run", "pygmentize", "-f", "html"] <> if T.null fileKind then [] else ["-l", convert fileKind])
           (convert str)
      transform x = return x
  in pandocCompilerWithTransformM
     defaultHakyllReaderOptions
     { readerExtensions = extensions
     }
     defaultHakyllWriterOptions
     { writerHTMLMathMethod = MathJax ""
     , writerSectionDivs = True
     , writerExtensions = extensions
     , writerHighlightStyle = Nothing
     }
     (bottomUpM transform . eastAsianLineBreakFilter) -- 東アジアの文字列に余計な空白が入らないようにする

entryContext :: Context String
entryContext = mconcat
  [ cleanUrlField
  , mconcat entryDate
  , constField "type" "article"
  , titleEscape
  , teaserFieldByResource 256 "teaser" "content" id
  , teaserFieldByResource 180 "og-description" "content" (convert . T.replace "\"" "&quot;" . convert)
  , defaultContext
  ]
  where titleEscape = field "title"
          (\item -> escapeHtml . fromJust <$> getMetadataField (itemIdentifier item) "title")
        entryDate = f <$> ["date", "published"]
          where f key = field key (\item -> do
                                      mMeta <- getMetadataField (itemIdentifier item) key
                                      case mMeta of
                                        Nothing -> return $ fromMaybe empty $ mItemDate item
                                        Just meta -> return meta
                                  )
        mItemDate item = case splitOneOf "-" f of
          [year, month, day, hour, minute, second] ->
            Just $ concat [year, "-", month, "-", day, "T", hour, ":", minute, ":", second, "+09:00"]
          [year, month, day] ->
            Just $ concat [year, "-", month, "-", day]
          _ -> Nothing
          where f = toFilePath $ cleanIdentifier $ itemIdentifier item
                cleanIdentifier = fromFilePath . dropExtension . takeFileName . toFilePath

teaserFieldByResource :: Int -> String -> Snapshot -> (String -> String) -> Context a
teaserFieldByResource size key snapshot escape = field key $ \item ->
  dropWarningHtmlEntity . take size . escape . stripTags . itemBody <$>
  loadSnapshot (itemIdentifier item) snapshot

dropWarningHtmlEntity :: String -> String
dropWarningHtmlEntity entity = R.subRegex (R.mkRegex "&[^&;]*$") entity ""

addTitleSuffix :: Context a
addTitleSuffix = field "title" $ \item ->
  (<> " - ncaq") . fromJust <$> getMetadataField (itemIdentifier item) "title"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle       = "ncaq"
  , feedDescription = "ncaq"
  , feedAuthorName  = "ncaq"
  , feedAuthorEmail = "ncaq@ncaq.net"
  , feedRoot        = "https://www.ncaq.net"
  }

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute `composeRoutes` customDateRoute
  where createIndexRoute ident = takeBaseName (dropExtension (toFilePath ident)) </>
          "index.html"

customDateRoute :: Routes
customDateRoute = customRoute (replaceDate . toFilePath)

replaceDate :: String -> String
replaceDate input = R.subRegex (R.mkRegex "-") input "/"

cleanUrlField :: Context a
cleanUrlField = field "url"
  (fmap (maybe empty $ (replaceDate . cleanUrlString) . toUrl) .
   getRoute . itemIdentifier)

cleanUrlString :: String -> String
cleanUrlString = cleanIndex
  where cleanIndex path | "/index.html" `isSuffixOf` path = dropFileName path
                        | otherwise = path

-- | HTMLとして正しいかをチェックだけして、
-- 内容は変更せずに返します。
tidyHtml :: Item String -> Compiler (Item String)
tidyHtml item = check item >> pure item
  where check =
          withItemBody $ unixFilter "tidy"
          [ "--errors"
          , "--mute-id", "y"
          , "--wrap", "0"
          , "--drop-empty-elements", "n"
          , "--tidy-mark", "n"
          -- 古いtidyではHTML属性情報が正しくないことがあるので、チェックを諦めます。
          , "--warn-proprietary-attributes", "n"
          ]

-- | XMLとして正しいかをチェックだけして、
-- 内容は変更せずに返します。
tidyXml :: Item String -> Compiler (Item String)
tidyXml item = check item >> pure item
  where check =
          withItemBody $ unixFilter "tidy"
          [ "--errors"
          , "--mute-id", "y"
          , "--wrap", "0"
          , "-xml"
          ]
