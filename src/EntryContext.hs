module EntryContext (entryContext) where

import BasicContext
import Data.List.Split qualified as L
import Escape
import Hakyll
import Himari hiding (Context)
import Teaser
import Timestamp
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
  -- 機械可読な`datetime`属性向けにISO 8601の値を残しつつ、
  -- 表示用に日本語表記へ変換したフィールドも提供する。
  entryDate =
    mconcat
      [ field "date" $ fmap (fromMaybe empty) . itemIsoDate
      , field "date-japanese" $ \item -> do
          mIso <- itemIsoDate item
          case mIso of
            Nothing -> pure empty
            Just isoDate -> toJapaneseOrFail isoDate
      , field "updated-japanese" $ \item -> do
          mUpdated <- getMetadataField (itemIdentifier item) "updated"
          case mUpdated of
            Nothing -> noResult "updatedメタデータが存在しない"
            Just updated -> toJapaneseOrFail updated
      ]
  -- ISO 8601形式の文字列を日本語表記に変換する。
  -- 解釈できない場合はフォールバックせずにビルドを失敗させる。
  toJapaneseOrFail isoDate =
    maybe (fail $ "日時を解釈できませんでした: " <> isoDate) pure $ formatJapanese isoDate
  -- 記事の作成日時をISO 8601形式で取得する。
  -- メタデータの`date`を優先し、無ければファイル名から導出する。
  itemIsoDate item = do
    mMeta <- getMetadataField (itemIdentifier item) "date"
    pure $ mMeta <|> mItemDate item
  mItemDate item = case L.splitOneOf "-" f of
    [year, month, day, hour, minute, second] ->
      Just $ concat [year, "-", month, "-", day, "T", hour, ":", minute, ":", second, "+09:00"]
    [year, month, day] ->
      Just $ concat [year, "-", month, "-", day]
    _ -> Nothing
   where
    f = toFilePath . cleanIdentifier $ itemIdentifier item
    cleanIdentifier = fromFilePath . dropExtension . takeFileName . toFilePath
