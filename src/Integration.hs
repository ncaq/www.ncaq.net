module Integration (integrateExec) where

import Browser
import Hakyll
import Himari
import Vite

-- | Hakyllの処理とViteなど外部プロセスの処理を統合してコマンドを実行します。
integrateExec :: Options -> IO () -> IO ()
integrateExec options runHakyll = do
  -- サイト生成を行うサブコマンドの場合のみ、
  -- Viteでスタイルシートをバンドルする。
  -- `site/dist/bundle.css`が`provideDirectory`配下に出力されるので、
  -- 後続のHakyllが通常通り検出してコピーできる。
  case optCommand options of
    Build{} -> runViteBuild >> runHakyll
    Rebuild -> runViteBuild >> runHakyll
    -- watch/serverではViteもwatchモードで動かして、
    -- スタイル変更時にバンドルを更新し続ける。
    -- 合わせてプレビューサーバが起動したらwebブラウザでプレビューを開く。
    Watch{host, port, no_server} -> do
      unless no_server $ void $ openBrowserWhenReady host port
      withViteWatch runHakyll
    Server{host, port} -> do
      void $ openBrowserWhenReady host port
      withViteWatch runHakyll
    _ -> runHakyll
