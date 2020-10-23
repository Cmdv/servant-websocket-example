module Lib.WSServer where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (forM_)
import Data.Text (pack)
import Lib.Core.AppMonad (AppEnv)
import Network.WebSockets (DataMessage(Text), DataMessage, ServerApp, Connection, withPingThread, sendTextData)
import Servant ((:>), Proxy (..), Server)
import Servant.API.WebSocket (WebSocket)
import Network.WebSockets.Connection (acceptRequest)
import Network.WebSockets.Connection (sendDataMessage)
import Data.Aeson (encode)

type WebSocketApi = "health" :> WebSocket

api :: Proxy WebSocketApi
api = Proxy

wsApp :: AppEnv -> Server WebSocketApi
wsApp env = streamData
 where
   streamData :: MonadIO m => Connection -> m ()
   streamData c = liftIO . forM_ [1..] $ \i -> do
     withPingThread c 10 (return ()) (sendTextData c (pack $ show (i :: Int)) >> threadDelay 1000000)
