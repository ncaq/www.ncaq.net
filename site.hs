{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid
import           Hakyll

main :: IO ()
main = hakyll $ do
    match "css/*" $ route idRoute >> compile compressCssCompiler
    match "images/*" $ route idRoute >> compile copyFileCompiler
    match "templates/*" $ compile templateCompiler

    match "entry/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler >>=
            loadAndApplyTemplate "templates/entry.html" entryContext >>=
            loadAndApplyTemplate "templates/default.html" entryContext >>=
            relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexContext = listField "entry" entryContext (loadAll "entry/*" >>= recentFirst) <>
                    defaultContext
            getResourceBody >>=
                applyAsTemplate indexContext >>=
                loadAndApplyTemplate "templates/default.html" indexContext >>=
                relativizeUrls

entryContext :: Context String
entryContext = dateField "date" "%F" <> defaultContext
