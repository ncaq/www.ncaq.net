module Timestamp
  ( formatJapanese
  ) where

import Data.Time.Format.ISO8601 (iso8601ParseM)
import Himari hiding (Context)

-- | ISO 8601形式の日時文字列を日本人向けの表記に変換する。
-- 日時を含む場合は @2025年12月30日(火)15時41分18秒@、
-- 日付のみの場合は @2025年12月30日(火)@ を返す。
-- 桁幅を常に揃えるため、月日と時分秒は2桁のゼロ埋めで表示する。
-- 解釈できない文字列の場合は'Nothing'を返す。
formatJapanese :: String -> Maybe String
formatJapanese s =
  (formatZonedTime <$> iso8601ParseM s)
    <|> (formatDay <$> iso8601ParseM s)

-- | 日時を日本語表記に変換する。
formatZonedTime :: ZonedTime -> String
formatZonedTime zonedTime =
  let localTime = zonedTimeToLocalTime zonedTime
   in formatDay (localDay localTime) <> formatTimeOfDay (localTimeOfDay localTime)

-- | 日付部分を @2025年12月30日(火)@ の形式に変換する。
formatDay :: Day -> String
formatDay day =
  let (year, month, dayOfMonth) = toGregorian day
   in mconcat
        [ show year
        , "年"
        , zeroPad2 month
        , "月"
        , zeroPad2 dayOfMonth
        , "日("
        , formatDayOfWeek (dayOfWeek day)
        , ")"
        ]

-- | 時刻部分を @15時41分18秒@ の形式に変換する。
formatTimeOfDay :: TimeOfDay -> String
formatTimeOfDay timeOfDay =
  mconcat
    [ zeroPad2 (todHour timeOfDay)
    , "時"
    , zeroPad2 (todMin timeOfDay)
    , "分"
    , zeroPad2 (floor (todSec timeOfDay) :: Int)
    , "秒"
    ]

-- | 整数を2桁のゼロ埋め文字列に変換する。
zeroPad2 :: Int -> String
zeroPad2 n =
  let s = show n
   in if length s < 2 then '0' : s else s

-- | 曜日を日本語1文字に変換する。
formatDayOfWeek :: DayOfWeek -> String
formatDayOfWeek Monday = "月"
formatDayOfWeek Tuesday = "火"
formatDayOfWeek Wednesday = "水"
formatDayOfWeek Thursday = "木"
formatDayOfWeek Friday = "金"
formatDayOfWeek Saturday = "土"
formatDayOfWeek Sunday = "日"
