module EntryAndYears (getEntryAndYears) where

import qualified Data.List as L
import System.Directory

-- | 年一覧をスマートにページネーションする方法がわからなかったので`IO`でごり押しする。
-- `Rules`は内部的には`IO`をベースに持つが、
-- unsafe系以外で持ち上げる方法が見つからないので、
-- 別コンテキストで処理する。
getEntryAndYears :: IO (String, [String])
getEntryAndYears = do
  entryIndex <- readFile "site/entry-index.html"
  years <- yearInEntry
  return (entryIndex, years)

-- | entryディレクトリ下にある記事が存在する年をリストで返却する。
-- 0埋めされてる可能性も考慮して`Int`ではなくあえて`String`。
-- Hakyllのフレームワークに乗りたかった気持ちはあるが、
-- メタデータではなく`Context`下で取り出す方法がよくわからなかった。
-- 規則性があるので今回はこれで問題ないと判断した。
yearInEntry :: IO [String]
yearInEntry = do
  entryList <- L.sort <$> listDirectory "site/entry"
  let getYear = L.takeWhile (/= '-')
  return $ reverse $ L.nub $ getYear <$> entryList
