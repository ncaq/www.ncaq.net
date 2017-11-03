{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.List           (isSuffixOf)
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set            as S
import           Hakyll
import           System.FilePath
import           Text.Pandoc
import qualified Text.Regex          as R

main :: IO ()
main = hakyllWith conf $ do
    match "templates/*" $ compile templateCompiler
    match ("*.ico" .||. "*.png" .||. "*.svg" .||. "*.txt" .||. "asset/*") $ do
        route idRoute
        compile copyFileCompiler

    match ("*.md" .||. "entry/*.md") $ do
        route cleanRoute
        compile $ pandocCompilerCustom >>=
            saveSnapshot "content" >>=
            loadAndApplyTemplate "templates/entry.html" entryContext >>=
            loadAndApplyTemplate "templates/default.html" (addTitleSuffix <> entryContext) >>=
            cleanUrls >>=
            indentHtml

    match "index.html" $ do
        route idRoute
        let indexContext = listField "entry" entryContext (reverse <$> loadAll "entry/*.md") <>
                constField "title" "ncaq" <>
                constField "date" "" <>
                constField "teaser" "index" <>
                defaultContext
        compile $ getResourceBody >>=
            applyAsTemplate indexContext >>=
            loadAndApplyTemplate "templates/default.html" indexContext >>=
            cleanUrls >>=
            indentHtml

    match "default.scss" $ do
        route $ setExtension "css"
        compile $ unixFilter "npm" ["run", "-s", "default.css"] "" >>= makeItem

    match "default.ts" $ do
        route $ setExtension "js"
        compile $ unixFilter "npm" ["run", "-s", "default.js"] "" >>= makeItem

    create ["feed.atom"] $ do
        route idRoute
        compile $ do
            let feedContext = entryContext <> bodyField "description"
            entry <- take 20 . reverse <$> loadAllSnapshots "entry/*.md" "content"
            renderAtom feedConfiguration feedContext entry >>=
                cleanUrls >>=
                indentXml

conf :: Configuration
conf = def { deployCommand = "rsync -vcazh --chmod=D755,F644 --delete --progress _site/ ncaq@ncaq.net:/var/www/www.ncaq.net" }

pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom = pandocCompilerWith
    defaultHakyllReaderOptions
    { readerExtensions = S.insert Ext_ignore_line_breaks
        $ readerExtensions defaultHakyllReaderOptions }
    defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    , writerSectionDivs = True
    , writerExtensions = S.insert Ext_ignore_line_breaks
        $ writerExtensions defaultHakyllWriterOptions
    , writerHtml5 = True
    }

entryContext :: Context String
entryContext = mconcat
    [ cleanUrlField
    , mconcat entryDate
    , teaserFieldByResource 195 "teaser" "content"
    , titleWbr
    , defaultContext
    ]
  where cleanUrlField = field "url"
            (fmap (maybe empty $ (replaceDate . cleanUrlString) . toUrl) . getRoute . itemIdentifier)
        entryDate = f <$> ["date", "published", "updated"]
          where f key = field key (\item -> do
                                          mMeta <- getMetadataField (itemIdentifier item) key
                                          case mMeta of
                                              Nothing -> return $ fromMaybe empty $ mItemDate item
                                              Just meta -> return meta
                                  )
        mItemDate item = let l = splitOneOf "-" f
                         in if 3 <= length l then Just f else Nothing
          where f = toFilePath $ cleanIdentifier $ itemIdentifier item
                cleanIdentifier = fromFilePath . dropExtension . takeFileName . toFilePath
        titleWbr = field "title_wbr"
            (\item -> (\mTitle -> R.subRegex (R.mkRegex ",") (fromJust mTitle) ",<wbr>") <$>
                getMetadataField (itemIdentifier item) "title")

teaserFieldByResource :: Int -> String -> Snapshot -> Context String
teaserFieldByResource l key snapshot = field key $ \item ->
    escapeHtml . take l . stripTags . itemBody <$> loadSnapshot (itemIdentifier item) snapshot

addTitleSuffix :: Context a
addTitleSuffix = field "title" (\item -> (<> " - ncaq") . fromJust
                                   <$> getMetadataField (itemIdentifier item) "title")

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
  where createIndexRoute ident = takeBaseName (dropExtension (toFilePath ident)) </> "index.html"

customDateRoute :: Routes
customDateRoute = customRoute (replaceDate . toFilePath)

replaceDate :: String -> String
replaceDate input = R.subRegex (R.mkRegex "^([0-9]+)-([0-9]+)-([0-9]+).") input "\\1/\\2/\\3/"

cleanUrls :: Item String -> Compiler (Item String)
cleanUrls = return . fmap (withUrls cleanUrlString)

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
