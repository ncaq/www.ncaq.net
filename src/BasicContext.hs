module BasicContext (basicContext) where

import Hakyll
import Route

-- | どのページにも共通する基本的な情報。
basicContext :: Context String
basicContext =
  let context =
        mconcat
          [ constField "root" "https://www.ncaq.net"
          , cleanUrlField
          , constField "og-image" "https://www.ncaq.net/favicon.png"
          , constField "twitter-creator" "@ncaq"
          , constField "twitter-site" "@ncaq"
          , defaultContext
          ]
   in mconcat
        -- JSON-LDやOpenGraphはtypeがハードコーディングされているからあえて入れない。
        [ twitterCardField "twitter" context
        , context
        ]
