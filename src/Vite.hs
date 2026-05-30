module Vite (runViteBuild, withViteWatch) where

import Himari

-- | `npm run build`を呼んでスタイルシートをバンドルする。
runViteBuild :: IO ()
runViteBuild = runProcess_ (proc "npm" ["run", "build"])

-- | `npm run watch`をバックグラウンドで起動して、
-- スタイル変更時にバンドルを更新し続ける。
-- Hakyllの処理が終わったら(あるいは例外で抜けたら)Viteも停止します。
-- watch起動直後はまだ初回ビルドが完了していないので、
-- 先に`npm run build`を同期実行してHakyllが起動直後にバンドルを参照できる状態にしておきます。
withViteWatch :: IO a -> IO a
withViteWatch action = do
  runViteBuild
  withProcessTerm (proc "npm" ["run", "watch"]) (const action)
