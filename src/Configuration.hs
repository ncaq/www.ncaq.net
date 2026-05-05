module Configuration (conf) where

import Data.Default
import Hakyll

-- | Hakyllに渡す設定。
conf :: Configuration
conf =
  def
    { providerDirectory = "site"
    , deployCommand = "wrangler pages deploy _site --project-name www-ncaq-net"
    }
