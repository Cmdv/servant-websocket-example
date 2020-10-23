module Lib.WSServer where

import Prelude
import Servant.API.WebSocket (WebSocket)

import Control.Concurrent       (threadDelay)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.Foldable            (forM_)
import Data.Text                (pack)
import Network.Wai              (Application)
import Network.WebSockets       (Connection, withPingThread, sendTextData)
import Servant                  ((:>), Proxy (..), Server, serve)

type WebSocketApi = "health" :> WebSocket

-- wsApp :: Application
-- wsApp = serve api server

api :: Proxy WebSocketApi
api = Proxy

wsApp :: AppEnv -> Server WebSocketApi
wsApp env = streamData
 where
   streamData :: MonadIO m => Connection -> m ()
   streamData c = liftIO . forM_ [1..] $ \i -> do
     withPingThread c 10 (return ()) (sendTextData c (pack $ show (i :: Int)) >> threadDelay 1000000)
