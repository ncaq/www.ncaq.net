{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

import           Control.Applicative
import           Data.List                      (isSuffixOf)
import           Data.List.Split
import           Data.Maybe
import           Hakyll
import           System.FilePath
import           Text.Pandoc
import qualified "regex-compat-tdfa" Text.Regex as R

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
      indentHtml

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
      indentHtml

  create ["sitemap.xml"] $ do
    route idRoute
    let sitemapContext =
          listField "entry" entryContext (reverse <$> loadAll ("*.md" .||. "entry/*.md"))
    compile $ getResourceBody >>=
      applyAsTemplate sitemapContext >>=
      indentXml

  create ["feed.atom"] $ do
    route idRoute
    compile $ do
      let feedContext = entryContext <> bodyField "description"
      entry <- take 20 . reverse <$> loadAllSnapshots "entry/*.md" "content"
      renderAtom feedConfiguration feedContext entry >>=
        indentXml

conf :: Configuration
conf = def
  { deployCommand = "rsync --verbose --checksum --archive --chmod=D755,F644 " <>
    "--delete --compress --human-readable --progress " <>
    "_site/ ncaq@ncaq.net:/var/www/www.ncaq.net"
  }

pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom
  = let extensions =
          -- 大したこと無いように見えて結構パフォーマンスに影響する
          disableExtension Ext_pandoc_title_block $
          -- 記号を変に変えられるのは困る
          disableExtension Ext_smart $
          -- 日本語だとスペースを入れると意味が変わってしまう
          enableExtension Ext_ignore_line_breaks $
          -- 一応自動見出しを入れる
          enableExtension Ext_auto_identifiers $
          readerExtensions defaultHakyllReaderOptions
        transform (CodeBlock (_identifier, classes, _keyValue) str)
          = let fileExtension = takeExtension (unwords classes)
                fileKind = if null fileExtension then unwords classes else tail fileExtension
            in RawBlock (Format "html") <$>
               unixFilter "pygmentize"
               (["-f", "html"] <>
               if null fileKind then [] else ["-l", fileKind])
               str
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
       (bottomUpM transform)

entryContext :: Context String
entryContext = mconcat
  [ cleanUrlField
  , mconcat entryDate
  , constField "type" "article"
  , titleEscape
  , titleWbr
  , teaserFieldByResource 500 "teaser" "content"
  , teaserFieldByResource 180 "og-description" "content"
  , defaultContext
  ]
  where titleEscape = field "title"
          (\item -> escapeHtml . fromJust <$> getMetadataField (itemIdentifier item) "title")
        titleWbr = field "title_wbr"
          (\item -> (\mTitle -> R.subRegex (R.mkRegex ",") (fromJust mTitle) ",<wbr>") <$>
            getMetadataField (itemIdentifier item) "title")
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

teaserFieldByResource :: Int -> String -> Snapshot -> Context String
teaserFieldByResource l key snapshot = field key $ \item ->
  dropWarningHtmlEntity . take l . stripTags . itemBody <$>
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

indentHtml :: Item String -> Compiler (Item String)
indentHtml = withItemBody $ unixFilter "tidy"
  [ "--drop-empty-elements", "n"
  , "--tidy-mark", "n"
  , "--wrap", "0"
  , "-indent"
  ]

indentXml :: Item String -> Compiler (Item String)
indentXml = withItemBody $ unixFilter "tidy"
  [ "--indent-cdata" , "y"
  , "--wrap", "0"
  , "-quiet"
  , "-xml"
  , "-indent"
  ]
