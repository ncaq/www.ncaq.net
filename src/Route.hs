module Route (cleanRoute, cleanUrlField) where

import Control.Applicative
import Data.Convertible
import qualified Data.List as L
import qualified Data.Text as T
import Hakyll
import System.FilePath

-- | ファイルパスのハイフンをディレクトリ区切りに変換して、
-- 末尾に`index.html`を付与してディレクトリのデフォルトとして参照されるようにする。
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute `composeRoutes` customHyphenToSlash
 where
  createIndexRoute ident = takeBaseName (dropExtension (toFilePath ident)) </> "index.html"

-- | ファイルパスのハイフンをディレクトリの区切り文字に変換する。
customHyphenToSlash :: Routes
customHyphenToSlash = customRoute (replaceHyphenToSlash . toFilePath)

-- | ハイフンをスラッシュに変換する。
replaceHyphenToSlash :: String -> String
replaceHyphenToSlash = convert . T.replace "-" "/" . convert

-- | urlフィールドにファイルからではない適切なurlを入力する。
cleanUrlField :: Context a
cleanUrlField =
  field
    "url"
    ( fmap (maybe empty $ (replaceHyphenToSlash . cleanUrlString) . toUrl)
        . getRoute
        . itemIdentifier
    )

-- | 末尾のindex.htmlを除去してパスだけで閲覧できるようにする。
cleanUrlString :: String -> String
cleanUrlString = cleanIndex
 where
  cleanIndex path
    | "/index.html" `L.isSuffixOf` path = dropFileName path
    | otherwise = path
