module Lib.WSServer where

import Control.Monad.Reader (liftIO)
import Data.Text (pack)
import Lib.Core.AppMonad (envStats, AppEnv)
import Lib.Core.Stats (RequestCount, requestCount)
import Lib.Effect.Stats (getStats, MonadStats)
import qualified Network.WebSockets as WS
import Control.Concurrent (threadDelay)
import Data.Foldable (forM_)
import Control.Concurrent.MVar (readMVar)

wsApp :: AppEnv -> WS.ServerApp
wsApp appEnv pending = do
  conn <- WS.acceptRequest pending
  -- loop getting request count every 2 seconds
  forM_ [1..] $ \_ -> do
    reqCount <- liftIO $ readMVar $ requestCount $ envStats appEnv
    WS.withPingThread
      conn
      10
      (return ())
      (WS.sendTextData conn (pack $ show reqCount) >> threadDelay 2000000)

getEnvStats :: (MonadStats m) => m RequestCount
getEnvStats = do
  reqCount <- getStats
  pure reqCount
