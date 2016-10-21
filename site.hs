{-# LANGUAGE OverloadedStrings #-}

import           Clay                hiding (empty, reverse)
import           Control.Applicative
import           Data.List           (isSuffixOf)
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import           Hakyll
import           System.FilePath
import           System.Process
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
            cleanUrls >>=
            indentHtml

    match "index.html" $ do
        route idRoute
        let indexContext = listField "entry" entryContext (reverse <$> loadAll "entry/*") <>
                defaultContext
        compile $ getResourceBody >>=
            applyAsTemplate indexContext >>=
            applyDefaultTemplate >>=
            cleanUrls >>=
            indentHtml

    create ["feed.atom"] $ do
        route idRoute
        compile $ do
            let feedContext = entryContext <> bodyField "description"
            entry <- loadAllSnapshots "entry/*" "content"
            renderAtom feedConfiguration feedContext entry >>=
                cleanUrls >>=
                indentXml

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
entryContext = mconcat [cleanUrlField, mconcat entryDate, defaultContext]
  where cleanUrlField = field "path" (fmap (maybe empty $ cleanUrlString . toUrl) .
                                     getRoute . itemIdentifier)
        entryDate = f <$> ["date", "published", "updated"]
          where f k = field k (pure . fromMaybe empty . mItemDate)
        mItemDate item = let l = splitOneOf "-" f
                         in if (3 <= length l) then Just f else Nothing
          where f = toFilePath $ cleanIdentifier $ itemIdentifier item
                cleanIdentifier = fromFilePath . dropExtension . takeFileName . toFilePath

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "www.ncaq.net"
    , feedDescription = "www.ncaq.net entry"
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
cleanUrlString = hyphenToSlash . cleanIndex
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
