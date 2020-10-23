module Lib.HttpServer.Server
       ( Api
       , httpApp
       ) where


import Data.Aeson.Types (ToJSON)
import Data.Data (Proxy(Proxy))
import GHC.Generics (Generic)
import Lib.Effect.Stats (getStats, putStats, MonadStats)
import Lib.Core.AppMonad (AppEnv, AppServer, ToApi)
import Network.Wai (Application)
import Servant (JSON, Post)
import Servant.API ((:>))
import Servant.API.Generic (toServant, GenericMode((:-)))
import Servant.Server (Server, hoistServer, serve)

type Api = ToApi HttpSite

-- | These are the routes to the http app
data HttpSite route = HttpSite
    { -- | Post Hello
      helloRoute :: route
        :- "hello"
        :> Post '[JSON] HelloRes
    } deriving (Generic)

newtype HelloRes = HelloRes { msg :: String }
  deriving Generic

instance ToJSON HelloRes

httpServer :: HttpSite AppServer
httpServer = HttpSite
    { helloRoute = helloHandler
    }

helloHandler :: ( MonadStats m ) => m HelloRes
helloHandler = do
    reqCount <- getStats
    let newReqCount = (reqCount + 1)
    putStats newReqCount
    pure $ HelloRes (show newReqCount)

server :: AppEnv -> Server Api
server env = hoistServer
    (Proxy @Api)
    (pure _)
    (toServant httpServer)

httpApp :: AppEnv -> Application
httpApp env = serve
    (Proxy @Api)
    (server env)