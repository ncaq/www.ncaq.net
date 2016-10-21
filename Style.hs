{-# LANGUAGE OverloadedStrings #-}

module Style where

import           Clay
import           Data.Monoid
import           Prelude     hiding (rem)

defaultCss :: Css
defaultCss = do
    body ? do
        backgroundColor base03
        color base0
        fontFamily ["Meiryo"] [sansSerif]
        fontSizeCustom large
        overflowWrap breakWord
    h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
        backgroundColor base02
    a ? do
        color sblue
        ":visited" & color sviolet
  where base03  = rgb   0  43  54
        base02  = rgb   7  54  66
        base01  = rgb  88 110 117
        base00  = rgb  01 123 131
        base0   = rgb 131 148 150
        sblue   = rgb  38 139 210
        sviolet = rgb 108 113 196
