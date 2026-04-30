module EntryContext (entryContext) where

import BasicContext
import Control.Applicative
import qualified Data.List.Split as L
import Data.Maybe
import Escape
import Hakyll
import System.FilePath
import Teaser
import Title

-- | Markdownの記事として独立しているページ向け。
entryContext :: Context String
entryContext =
  let context =
        mconcat
          [ titleEscape
          , entryDate
          , teaserFieldByResource 256 "teaser" "content" id
          , teaserFieldByResource 100 "description" "content" escapeDoubleQuote
          , teaserFieldByResource 180 "og-description" "content" escapeDoubleQuote
          , basicContext
          ]
   in mconcat
        [ context
        , openGraphField "opengraph" context
        ]
 where
  titleEscape = mapTitleEx (fmap escapeHtml)
  entryDate =
    field
      "date"
      $ \item -> do
        mMeta <- getMetadataField (itemIdentifier item) "date"
        case mMeta of
          Nothing -> return $ fromMaybe empty $ mItemDate item
          Just meta -> return meta
  mItemDate item = case L.splitOneOf "-" f of
    [year, month, day, hour, minute, second] ->
      Just $ concat [year, "-", month, "-", day, "T", hour, ":", minute, ":", second, "+09:00"]
    [year, month, day] ->
      Just $ concat [year, "-", month, "-", day]
    _ -> Nothing
   where
    f = toFilePath $ cleanIdentifier $ itemIdentifier item
    cleanIdentifier = fromFilePath . dropExtension . takeFileName . toFilePath
