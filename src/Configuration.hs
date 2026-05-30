module Configuration (conf) where

import Hakyll
import Himari

-- | Hakyllに渡す設定。
conf :: Configuration
conf =
  def
    { providerDirectory = "site"
    , deployCommand = "wrangler pages deploy _site --project-name www-ncaq-net"
    }
