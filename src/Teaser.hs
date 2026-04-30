module Teaser (teaserFieldByResource) where

import Escape
import Hakyll

-- | 記事一覧などに利用するために記事の冒頭を指定された文字数で切り取ってキーに設定する。
teaserFieldByResource :: Int -> String -> Snapshot -> (String -> String) -> Context a
teaserFieldByResource size key snapshot escape = field key $ \item ->
  dropWarningHtmlEntity . take size . escape . stripTags . itemBody
    <$> loadSnapshot (itemIdentifier item) snapshot
