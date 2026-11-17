module EntryAndYears (getEntryAndYears) where

import Data.Containers.ListUtils qualified as L
import Data.List qualified as L
import Data.Text.IO.Utf8 qualified as T
import Himari

-- | 年一覧をスマートにページネーションする方法がわからなかったので`IO`でごり押しする。
-- `Rules`は内部的には`IO`をベースに持つが、
-- unsafe系以外で持ち上げる方法が見つからないので、
-- 別コンテキストで処理する。
-- HakyllのAPIが`String`を要求するので読み込んだ内容は`String`に変換して返す。
getEntryAndYears :: (MonadIO m) => m (String, [String])
getEntryAndYears = do
  entryIndex <- liftIO $ T.readFile "site/entry-index.html"
  years <- yearInEntry
  pure (convert entryIndex, years)

-- | entryディレクトリ下にある記事が存在する年をリストで返却する。
-- 0埋めされてる可能性も考慮して`Int`ではなくあえて`String`。
-- Hakyllのフレームワークに乗りたかった気持ちはあるが、
-- メタデータではなく`Context`下で取り出す方法がよくわからなかった。
-- 規則性があるので今回はこれで問題ないと判断した。
yearInEntry :: (MonadIO m) => m [String]
yearInEntry = do
  entryList <- L.sort <$> listDirectory "site/entry"
  let getYear = L.takeWhile (/= '-')
  pure . reverse . L.nubOrd $ getYear <$> entryList
