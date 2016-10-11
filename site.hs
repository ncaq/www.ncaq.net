{-# LANGUAGE OverloadedStrings #-}

import           Data.List
import           Data.Monoid
import           Hakyll
import           System.FilePath

main :: IO ()
main = hakyll $ do
    match "css/*" $ route idRoute >> compile compressCssCompiler
    match "images/*" $ route idRoute >> compile copyFileCompiler
    match "templates/*" $ compile templateCompiler

    match "entry/*" $ do
        route cleanRoute
        compile $ pandocCompiler >>=
            loadAndApplyTemplate "templates/entry.html" entryContext >>=
            loadAndApplyTemplate "templates/default.html" entryContext >>=
            relativizeUrls >>=
            cleanIndexUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            entry <- loadAll "entry/*" >>= recentFirst
            let indexContext = listField "entry" entryContext (return entry) <> defaultContext
            getResourceBody >>=
                applyAsTemplate indexContext >>=
                loadAndApplyTemplate "templates/default.html" indexContext >>=
                relativizeUrls

entryContext :: Context String
entryContext = dateField "date" "%F" <> defaultContext

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
