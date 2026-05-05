module Title
  ( addTitleWithSuffix
  , mapTitleEx
  ) where

import Data.Maybe
import Hakyll

-- | サイトの名前込みのタイトルを設定する。
addTitleWithSuffix :: Context a
addTitleWithSuffix = mapTitleEx (fmap (<> " - ncaq"))

-- | フィールドのtitleデータを編集します。
-- タイトルが無い場合はエラーを出力して終了します。
mapTitleEx :: (MonadMetadata f) => (f String -> Compiler String) -> Context a
mapTitleEx f = field "title" (f . getTitleEx)

-- | メタデータからtitleを取得します。
-- タイトルが無い場合はエラーを出力して終了します。
getTitleEx :: (MonadMetadata f) => Item a -> f String
getTitleEx item =
  fromMaybe (error "title not found")
    <$> getMetadataField (itemIdentifier item) "title"
