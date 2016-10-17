{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.List
import           Data.Monoid
import           Hakyll
import           System.FilePath
import           Text.Pandoc

main :: IO ()
main = hakyll $ do
    match "css/*" $ route idRoute >> compile compressCssCompiler
    match "favicon.*" $ route idRoute >> compile copyFileCompiler
    match "file/*" $ route idRoute >> compile copyFileCompiler
    match "templates/*" $ compile templateCompiler

    match "entry/*" $ do
        route cleanRoute
        compile $ pandocCompilerCustom >>=
            loadAndApplyTemplate "templates/entry.html" entryContext >>=
            saveSnapshot "content" >>=
            loadAndApplyTemplate "templates/default.html" entryContext >>=
            cleanIndexUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexContext = listField "entry" entryContext (loadAll "entry/*") <> defaultContext
            getResourceBody >>=
                applyAsTemplate indexContext >>=
                loadAndApplyTemplate "templates/default.html" indexContext >>=
                cleanIndexUrls
    match "*.md" $ do
        route cleanRoute
        compile $ pandocCompilerCustom >>=
            saveSnapshot "content" >>=
            defaultTemplate >>=
            cleanIndexUrls

    create ["feed.atom"] $ do
        route idRoute
        compile $ do
            let feedContext = entryContext <> bodyField "description"
            entry <- loadAllSnapshots "entry/*" "content"
            renderAtom feedConfiguration feedContext entry >>= cleanIndexUrls

pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom = pandocCompilerWith defaultHakyllReaderOptions
    defaultHakyllWriterOptions { writerStandalone = True
                               , writerTemplate = unlines ["$toc$", "$body$"]
                               , writerNumberSections = True
                               , writerTableOfContents = True
                               , writerSectionDivs = True
                               , writerHtml5 = True
                               }

entryContext :: Context String
entryContext = field "published" fileDate <> field "date" fileDate <> field "updated" fileDate <>
    field "url" (fmap (maybe empty (cleanIndex . toUrl)) . getRoute . itemIdentifier) <>
    defaultContext

fileDate :: Item String -> Compiler String
fileDate = pure . toFilePath . cleanIdentifier . itemIdentifier

cleanIdentifier :: Identifier -> Identifier
cleanIdentifier = fromFilePath . dropExtension . takeFileName . toFilePath

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "www.ncaq.net"
    , feedDescription = "www.ncaq.net entry"
    , feedAuthorName  = "ncaq"
    , feedAuthorEmail = "ncaq@ncaq.net"
    , feedRoot        = "https://www.ncaq.net"
    }

-- | based on <https://github.com/crodjer/rohanjain.in/blob/master/site.hs>
-- <https://www.rohanjain.in/hakyll-clean-urls/>

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where createIndexRoute ident = takeBaseName (dropExtension (toFilePath ident)) </> "index.html"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndex :: String -> String
cleanIndex url | "/index.html" `isSuffixOf` url = dropFileName url
               | otherwise = url
