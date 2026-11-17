module Runner (Runner (..), getRunner) where

import Configuration
import EntryAndYears
import Hakyll
import Himari
import Run

-- | サイトのビルドを実行するための情報。
data Runner = Runner
  { options :: Options
  -- ^ Hakyllのコマンドライン引数の解析結果。
  , runHakyll :: IO ()
  -- ^ 解析結果を元にHakyllを実行するアクション。
  }

-- | サイトのビルドに必要なデータを入手。
getRunner :: (MonadIO m) => m Runner
getRunner = do
  options <- liftIO $ defaultParser conf
  (entryIndex, years) <- getEntryAndYears
  let runHakyll = hakyllRun options (entryIndex, years)
  pure $ Runner{options, runHakyll}
