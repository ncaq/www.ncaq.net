module Tidy (tidyHtml, tidyXml) where

import Hakyll

-- | HTMLとして正しいかをチェックだけして、
-- 内容は変更せずに返す。
tidyHtml :: Item String -> Compiler (Item String)
tidyHtml item = check item >> pure item
 where
  check =
    withItemBody $
      unixFilter
        "tidy"
        [ "--errors"
        , "--mute-id"
        , "y"
        , "--wrap"
        , "0"
        , "--drop-empty-elements"
        , "n"
        , "--tidy-mark"
        , "n"
        , -- 古いtidyではHTML属性情報が正しくないことがあるので、チェックを諦めます。
          "--warn-proprietary-attributes"
        , "n"
        ]

-- | XMLとして正しいかをチェックだけして、
-- 内容は変更せずに返す。
tidyXml :: Item String -> Compiler (Item String)
tidyXml item = check item >> pure item
 where
  check =
    withItemBody $
      unixFilter
        "tidy"
        [ "--errors"
        , "--mute-id"
        , "y"
        , "--wrap"
        , "0"
        , "-xml"
        ]
