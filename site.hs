{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.List
import           Data.List.Split
import           Data.Maybe
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

    match "**.md" $ do
        route cleanRoute
        compile $ pandocCompilerCustom >>=
            loadAndApplyTemplate "templates/entry.html" entryContext >>=
            saveSnapshot "content" >>=
            applyDefaultTemplate >>=
            cleanUrls

    match "index.html" $ do
        route idRoute
        let indexContext = listField "entry" entryContext (reverse <$> loadAll "entry/*") <>
                defaultContext
        compile $ getResourceBody >>=
            applyAsTemplate indexContext >>=
            applyDefaultTemplate >>=
            cleanUrls

    create ["feed.atom"] $ do
        route idRoute
        compile $ do
            let feedContext = entryContext <> bodyField "description"
            entry <- loadAllSnapshots "entry/*" "content"
            renderAtom feedConfiguration feedContext entry >>= cleanUrls

pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom = pandocCompilerWith defaultHakyllReaderOptions
    defaultHakyllWriterOptions { writerStandalone = True
                               , writerTemplate = unlines ["$toc$", "$body$"]
                               , writerNumberSections = True
                               , writerTableOfContents = True
                               , writerSectionDivs = True
                               , writerHtml5 = True
                               }

applyDefaultTemplate :: Item String -> Compiler (Item String)
applyDefaultTemplate = loadAndApplyTemplate "templates/default.html" defaultContext

entryContext :: Context String
entryContext = mconcat entryDate <> mconcat [defaultContext]
  where entryDate = f <$> ["date", "published", "updated"]
          where f k = field k (pure . fromMaybe empty . mItemDate)
        mItemDate i = let l = splitOneOf "-" f
                      in if (3 <= length l) then Just f else Nothing
          where f = toFilePath $ cleanIdentifier $ itemIdentifier i
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
cleanRoute = customRoute createIndexRoute `composeRoutes` customRoute (hyphenToSlash . toFilePath)
  where createIndexRoute ident = takeBaseName (dropExtension (toFilePath ident)) </> "index.html"

cleanUrls :: Item String -> Compiler (Item String)
cleanUrls = return . fmap (withUrls cleanUrlString)

cleanUrlString :: String -> String
cleanUrlString = hyphenToSlash . cleanIndex
  where cleanIndex url | "/index.html" `isSuffixOf` url = dropFileName url
                       | otherwise = url

hyphenToSlash :: String -> String
hyphenToSlash url = (\c -> if c == '-' then '/' else c) <$> url
