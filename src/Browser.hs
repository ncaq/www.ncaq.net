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

-- | 指定したポートにTCP接続できるようになるまで一定間隔で待ちます。
-- 接続できたら`True`、
-- 規定回数試しても駄目なら`False`を返します。
waitForPort :: String -> Int -> IO Bool
waitForPort host port = go 600
 where
  go :: Int -> IO Bool
  go n
    | n <= 0 = pure False
    | otherwise = do
        connected <- tryConnectToServer host port
        case connected of
          Right () -> return True
          _ -> threadDelay 100000 >> go (n - 1)

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
