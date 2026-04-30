module IndexContext (indexContext) where

import BasicContext
import Hakyll

-- | Markdownの記事ではなくHTMLから生成する一覧ページ。
indexContext :: String -> String -> String -> Context String -> Context String
indexContext title description ogType entryIndexField =
  mconcat
    [ constField "title" title
    , constField "description" description
    , constField "og-type" ogType
    , constField "og-description" description
    , entryIndexField
    , basicContext
    ]
