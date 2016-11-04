{-# LANGUAGE OverloadedStrings #-}

module Style where

import           Clay
import qualified Clay.Media  as M
import           Data.Monoid
import           Prelude     hiding (all, div, rem, (**))

defaultCss :: Css
defaultCss = do
    body ? do
        backgroundColor base03
        color base0
        borderColor base01
        fontFamily ["Meiryo"] [sansSerif]
        overflowWrap breakWord
    header <> footer <> ".toc" ? border solid (px 1) base01
    ".toc" ? display displayTable
    h1 <> h2 <> h3 <> h4 <> h5 <> h6 <> blockquote <> pre <> code ? do
        backgroundColor base02
        color base1
        borderColor base0
    h1 ? fontSizeCustom xxLarge
    h2 ? fontSizeCustom xLarge
    h3 ? fontSizeCustom large
    h4 <> h5 <> h6 ? fontSizeCustom medium
    img ? maxWidth (pct 100)
    pre <> code ? do
        fontFamily ["Ricty", "Inconsolata", "Consolas"] [monospace]
        border solid (px 1) base0
    a ? do
        color sblue
        ":visited" & color sviolet
    li ** img ? maxHeight (ex 4)
    ".flex" ? flexbody
    ".flex" |> li ? marginRight (rem 1)
    query all [M.minWidth (px 768)] $ do
        ".flex-d" ? flexbody
        ".flex-d" |> div ? minWidth (pct 70)
        ".flex-d" |> footer ? minWidth (pct 30)
  where base03  = rgb   0  43  54
        base02  = rgb   7  54  66
        base01  = rgb  88 110 117
        base0   = rgb 131 148 150
        base1   = rgb 147 161 161
        sblue   = rgb  38 139 210
        sviolet = rgb 108 113 196
        flexbody = do
            display flex
            justifyContent spaceBetween
            listStyleType none
