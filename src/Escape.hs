module Escape
  ( dropWarningHtmlEntity
  , escapeDoubleQuote
  ) where

import Data.Convertible
import qualified Data.Text as T
import Text.Regex.TDFA

-- | HTMLエスケープされた記号が中途半端に残って別の意味になるのを防ぐため、
-- 切り取った箇所の末尾がHTMLエスケープっぽかったら削除する。
-- 実装が非常に雑。
dropWarningHtmlEntity :: String -> String
dropWarningHtmlEntity entity =
  let (safety, _match, _after) = entity =~ ("&[^&;]*$" :: String) :: (String, String, String)
   in safety

-- | ダブルクオートを雑にエスケープする。
-- 雑すぎるのでユーザが入力する可能性のある値を入れる場合許容できないが、
-- 今回は私が自分で入力する値だけが入るので許容する。
escapeDoubleQuote :: String -> String
escapeDoubleQuote = convert . T.replace "\"" "&quot;" . convert
