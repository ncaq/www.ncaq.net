{-# LANGUAGE OverloadedStrings #-}

import           Data.List
import           Data.Monoid
import           Hakyll
import           System.FilePath
import           Text.Pandoc

main :: IO ()
main = hakyll $ do
    match "css/*" $ route idRoute >> compile compressCssCompiler
    match "file/*" $ route idRoute >> compile copyFileCompiler
    match "templates/*" $ compile templateCompiler

    match "entry/*" $ do
        route cleanRoute
        compile $ pandocCompilerCustom >>=
            loadAndApplyTemplate "templates/entry.html" entryContext >>=
            loadAndApplyTemplate "templates/default.html" entryContext >>=
            relativizeUrls >>=
            cleanIndexUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexContext = listField "entry" entryContext (loadAll "entry/*") <> defaultContext
            getResourceBody >>=
                applyAsTemplate indexContext >>=
                loadAndApplyTemplate "templates/default.html" indexContext >>=
                relativizeUrls

pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom = pandocCompilerWith defaultHakyllReaderOptions
    defaultHakyllWriterOptions { writerNumberSections = True
                               , writerTableOfContents = True
                               , writerSectionDivs = True
                               , writerHtml5 = True
                               }

entryContext :: Context String
entryContext = field "date" fileDate <> defaultContext

fileDate :: Item String -> Compiler String
fileDate = pure . toFilePath . cleanIdentifier . itemIdentifier

cleanIdentifier :: Identifier -> Identifier
cleanIdentifier = fromFilePath . dropExtension . takeFileName . toFilePath

-- | based on <https://github.com/crodjer/rohanjain.in/blob/master/site.hs>
-- <https://www.rohanjain.in/hakyll-clean-urls/>

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where createIndexRoute ident = takeBaseName (dropExtension (toFilePath ident)) </> "index.html"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"
