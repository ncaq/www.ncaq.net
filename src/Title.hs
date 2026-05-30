module Title
  ( addTitleWithSuffix
  , mapTitleEx
  ) where

import Hakyll
import Himari hiding (Context)

-- | サイトの名前込みのタイトルを設定する。
addTitleWithSuffix :: Context a
addTitleWithSuffix = mapTitleEx (fmap (<> " - ncaq"))

-- | フィールドのtitleデータを編集します。
-- タイトルが無い場合はエラーを出力して終了します。
mapTitleEx :: (MonadFail f, MonadMetadata f) => (f String -> Compiler String) -> Context a
mapTitleEx f = field "title" (f . getTitleEx)

-- | メタデータからtitleを取得します。
-- タイトルが無い場合はエラーを出力して終了します。
getTitleEx :: (MonadFail f, MonadMetadata f) => Item a -> f String
getTitleEx item = do
  mTitle <- getMetadataField (itemIdentifier item) "title"
  case mTitle of
    Nothing -> fail "title not found"
    Just title -> return title
