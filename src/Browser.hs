module Browser
  ( openBrowserWhenReady
  ) where

import Himari
import Network.Socket

-- | プレビューサーバが起動した後にwebブラウザでプレビューを開きます。
-- Hakyllのプレビューサーバ起動はブロッキングなので別スレッドで処理します。
-- ポートに接続できるようになるまで待ってからブラウザを起動することで、
-- サーバの初回ビルド完了前にブラウザを開いてしまうことを防ぎます。
openBrowserWhenReady :: String -> Int -> IO (Async ())
openBrowserWhenReady host port = async $ do
  ready <- waitForPort host port
  if ready
    then openBrowser host port
    else throwM $ userError "サーバの起動に失敗しました"

-- | 指定したポートにTCP接続できるようになるまでリトライしつつ一定時間待ちます。
-- 接続できたら`True`、
-- 規定時間試しても駄目なら`False`を返します。
-- ネットワーク接続待ちの定番として、
-- 初期50ms・最大1sの指数バックオフで累積60秒まで再試行する設定にしています。
waitForPort :: String -> Int -> IO Bool
waitForPort host port = do
  let policy =
        limitRetriesByCumulativeDelay 60_000_000
          . capDelay 1_000_000
          $ exponentialBackoff 50_000
  isRight
    <$> retrying
      policy
      (\_ r -> pure $ isLeft r)
      (\_ -> tryConnectToServer host port)

-- | サーバへの接続を一度だけ試みます。
tryConnectToServer :: (Show a) => HostName -> a -> IO (Either SomeException ())
tryConnectToServer host port = try $ do
  let hints = defaultHints{addrSocketType = Stream}
  addrs <- getAddrInfo (Just hints) (Just host) (Just (show port))
  case addrs of
    [] -> throwM $ userError "アドレス解決に失敗しました"
    addr : _ ->
      bracket
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        close
        (`connect` addrAddress addr)

-- | URLを開きます。
openBrowser :: String -> Int -> IO ()
openBrowser host port = do
  let url = "http://" <> host <> ":" <> show port
  exitCode <- runProcess $ proc "xdg-open" [url]
  unless (exitCode == ExitSuccess) . throwM . userError $
    "ブラウザの起動に失敗しました" <> " (exit code: " <> show exitCode <> ")"
