{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.List           (isSuffixOf)
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set            as S
import           Data.Text.Lazy      (unpack)
import           Hakyll
import           System.FilePath
import           System.Process
import           Text.Pandoc

main :: IO ()
main = hakyllWith conf $ do
    match "favicon.*" $ route idRoute >> compile copyFileCompiler
    match "file/**" $ route idRoute >> compile copyFileCompiler
    match "templates/*" $ compile templateCompiler

    match ("*.md" .||. "entry/*.md") $ do
        route cleanRoute
        compile $ pandocCompilerCustom >>=
            loadAndApplyTemplate "templates/entry.html" entryContext >>=
            saveSnapshot "content" >>=
            loadAndApplyTemplate "templates/default.html" (addTitleSuffix <> entryContext) >>=
            cleanUrls >>=
            indentHtml

    match "index.html" $ do
        route idRoute
        let indexContext = listField "entry" entryContext (reverse <$> loadAll "entry/*") <>
                constField "title" "ncaq" <>
                defaultContext
        compile $ getResourceBody >>=
            applyAsTemplate indexContext >>=
            loadAndApplyTemplate "templates/default.html" indexContext >>=
            cleanUrls >>=
            indentHtml

    match "default.scss" $ do
        route $ setExtension "css"
        compile $ unsafeCompiler (readProcess "npm" ["run", "-s", "default.css"] "") >>= makeItem

    match "default.ts" $ do
        route $ setExtension "js"
        compile $ unsafeCompiler (readProcess "npm" ["run", "-s", "default.js"] "") >>= makeItem

    create ["feed.atom"] $ do
        route idRoute
        compile $ do
            let feedContext = entryContext <> bodyField "description"
            entry <- reverse <$> loadAllSnapshots "entry/*" "content"
            renderAtom feedConfiguration feedContext entry >>=
                cleanUrls >>=
                indentXml

conf :: Configuration
conf = def
    { deployCommand = "rsync -vcazh --delete --progress _site/ ncaq@ncaq.net:/var/www/www.ncaq.net"
    }

pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom = pandocCompilerWith
    defaultHakyllReaderOptions { readerExtensions = S.insert Ext_ignore_line_breaks $ readerExtensions defaultHakyllReaderOptions }
    defaultHakyllWriterOptions { writerStandalone = True
                               , writerTemplate = unlines [ "<div class=\"toc\">$toc$</div>"
                                                          , "$body$"]
                               , writerNumberSections = True
                               , writerTableOfContents = True
                               , writerSectionDivs = True
                               , writerExtensions = S.insert Ext_ignore_line_breaks $ writerExtensions defaultHakyllWriterOptions
                               , writerHtml5 = True
                               }

entryContext :: Context String
entryContext = mconcat [cleanUrlField, mconcat entryDate, defaultContext]
  where cleanUrlField = field "url" (fmap (maybe empty $ (hyphenToSlash . cleanUrlString) . toUrl) .
                                     getRoute . itemIdentifier)
        entryDate = f <$> ["date", "published", "updated"]
          where f k = field k (pure . fromMaybe empty . mItemDate)
        mItemDate item = let l = splitOneOf "-" f
                         in if (3 <= length l) then Just f else Nothing
          where f = toFilePath $ cleanIdentifier $ itemIdentifier item
                cleanIdentifier = fromFilePath . dropExtension . takeFileName . toFilePath

addTitleSuffix :: Context a
addTitleSuffix = field "title" (\item -> (<> " - ncaq") . fromJust <$>
                                   getMetadataField (itemIdentifier item) "title")

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "ncaq"
    , feedDescription = "ncaq"
    , feedAuthorName  = "ncaq"
    , feedAuthorEmail = "ncaq@ncaq.net"
    , feedRoot        = "https://www.ncaq.net"
    }

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute `composeRoutes` customRoute (hyphenToSlash . toFilePath)
  where createIndexRoute ident = takeBaseName (dropExtension (toFilePath ident)) </> "index.html"

cleanUrls :: Item String -> Compiler (Item String)
cleanUrls = return . fmap (withUrls cleanUrlString)

cleanUrlString :: String -> String
cleanUrlString = cleanIndex
  where cleanIndex path | "/index.html" `isSuffixOf` path = dropFileName path
                        | otherwise = path

hyphenToSlash :: String -> String
hyphenToSlash path = (\c -> if c == '-' then '/' else c) <$> path

indentHtml :: Item String -> Compiler (Item String)
indentHtml = withItemBody (\bo -> unsafeCompiler $ (\(_, o, _) -> o) <$>
                              readProcessWithExitCode "tidy"
                              [ "--tidy-mark", "n"
                              , "--wrap", "0"
                              , "-indent"
                              ] bo)

indentXml :: Item String -> Compiler (Item String)
indentXml = withItemBody (\bo -> unsafeCompiler $ (\(_, o, _) -> o) <$>
                             readProcessWithExitCode "tidy"
                             [ "--indent-cdata" , "y"
                             , "-xml"
                             , "-indent"
                             ] bo)
