module Runner (Runner (..), getRunner) where

import Configuration
import EntryAndYears
import Hakyll
import Run

data Runner = Runner
  { options :: Options
  , runHakyll :: IO ()
  }

-- | サイトのビルドに必要なデータを入手。
getRunner :: IO Runner
getRunner = do
  options <- defaultParser conf
  (entryIndex, years) <- getEntryAndYears
  let runHakyll = hakyllRun options (entryIndex, years)
  return $ Runner{options, runHakyll}
