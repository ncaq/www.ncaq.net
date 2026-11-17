module Browser
  ( BrowserException (..)
  , openBrowserWhenReady
  ) where

import Himari
import Network.Socket

-- | プレビューをwebブラウザで開く処理で発生する例外。
data BrowserException
  = -- | 規定時間待ってもサーバに接続できなかった。
    ServerNotReady
      HostName
      -- ^ 接続しようとしたホスト名。
      Int
      -- ^ 接続しようとしたポート番号。
  | -- | ホスト名からアドレスを解決できなかった。
    AddressNotResolved
      HostName
      -- ^ 解決しようとしたホスト名。
      Int
      -- ^ 接続しようとしたポート番号。
  | -- | ブラウザを起動するコマンドが失敗した。
    BrowserLaunchFailed
      String
      -- ^ ブラウザで開こうとしたURL。
      ExitCode
      -- ^ ブラウザを起動するコマンドの終了コード。
  deriving stock (Show)

instance Exception BrowserException

-- | プレビューサーバが起動した後にwebブラウザでプレビューを開きます。
-- Hakyllのプレビューサーバ起動はブロッキングなので別スレッドで処理します。
-- ポートに接続できるようになるまで待ってからブラウザを起動することで、
-- サーバの初回ビルド完了前にブラウザを開いてしまうことを防ぎます。
-- ブラウザが開けなくてもプレビューサーバ自体は使えるので、
-- 失敗は標準エラー出力にログを出すだけで呼び出し元へは伝播しません。
openBrowserWhenReady :: (MonadThrow m, MonadUnliftIO m) => HostName -> Int -> m (Async ())
openBrowserWhenReady host port = async . handleAny logFailure $ do
  ready <- waitForPort host port
  if ready
    then openBrowser host port
    else throwM $ ServerNotReady host port
 where
  logFailure e =
    runStderrLoggingT . logErrorN $
      "ブラウザでプレビューを開けませんでした: " <> convert (displayException e)

-- | 指定したポートにTCP接続できるようになるまでリトライしつつ一定時間待ちます。
-- 接続できたら`True`、
-- 規定時間試しても駄目なら`False`を返します。
-- ネットワーク接続待ちの定番として、
-- 初期50ms・最大1sの指数バックオフで累積60秒まで再試行する設定にしています。
waitForPort :: (MonadUnliftIO m) => HostName -> Int -> m Bool
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
tryConnectToServer :: (MonadUnliftIO m) => HostName -> Int -> m (Either SomeException ())
tryConnectToServer host port = try . liftIO $ do
  let hints = defaultHints{addrSocketType = Stream}
  addrs <- getAddrInfo (Just hints) (Just host) (Just (show port))
  case addrs of
    [] -> throwM $ AddressNotResolved host port
    addr : _ ->
      bracket
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        close
        (`connect` addrAddress addr)

-- | URLを開きます。
openBrowser :: (MonadIO m, MonadThrow m) => HostName -> Int -> m ()
openBrowser host port = do
  let url = "http://" <> host <> ":" <> show port
  exitCode <- runProcess $ proc "xdg-open" [url]
  unless (exitCode == ExitSuccess) . throwM $ BrowserLaunchFailed url exitCode
