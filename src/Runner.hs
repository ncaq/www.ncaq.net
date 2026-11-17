module Runner (Runner (..), getRunner) where

import Configuration
import EntryAndYears
import Hakyll
import Himari
import Run

-- | コマンドライン引数の解析結果と、
-- それを元にHakyllを実行するアクションの組。
data Runner = Runner
  { options :: Options
  , runHakyll :: IO ()
  }

-- | サイトのビルドに必要なデータを入手。
getRunner :: (MonadIO m) => m Runner
getRunner = do
  options <- liftIO $ defaultParser conf
  (entryIndex, years) <- getEntryAndYears
  let runHakyll = hakyllRun options (entryIndex, years)
  pure $ Runner{options, runHakyll}
