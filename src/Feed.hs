module Feed (feedConfiguration) where

import Hakyll

-- | RSS Feed配信向けの設定。
feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "ncaq"
    , feedDescription = "www.ncaq.net entry"
    , feedAuthorName = "ncaq"
    , feedAuthorEmail = "ncaq@ncaq.net"
    , feedRoot = "https://www.ncaq.net"
    }
