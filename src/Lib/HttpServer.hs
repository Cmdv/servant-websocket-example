module Lib.HttpServer
       ( Api
       , httpApp
       ) where


import Data.Aeson.Types (ToJSON)
import Data.Data (Proxy(Proxy))
import GHC.Generics (Generic)
import Lib.Effect.Stats (getStats, putStats, MonadStats)
import Lib.Core.AppMonad (runAppAsHandler, AppEnv, AppServer, ToApi)
import Network.Wai (Application)
import Servant (JSON, Get)
import Servant.API ((:>))
import Servant.API.Generic (toServant, GenericMode((:-)))
import Servant.Server (Server, hoistServer, serve)
import Data.Aeson (FromJSON)

type Api = ToApi HttpSite

-- | These are the routes to the http app
data HttpSite route = HttpSite
    { -- | Post Health
      healthRoute :: route
        :- "health"
        :> Get '[JSON] HealthRes
    } deriving Generic

-- type used for the JSON responce
newtype HealthRes = HealthRes { msg :: String }
  deriving (Generic, Show)
  deriving newtype (FromJSON, ToJSON)

httpServer :: HttpSite AppServer
httpServer = HttpSite
  { healthRoute = healthHandler
  }

-- Handlers used when dealing with specific endpoints
-- constraint to MonadStats m to get access to that state
healthHandler :: MonadStats m => m HealthRes
healthHandler = do
    reqCount <- getStats
    let newReqCount = (reqCount + 1)
    putStats newReqCount
    pure $ HealthRes $ show newReqCount

-- Entry point into the http server
server :: AppEnv -> Server Api
server env = hoistServer
    (Proxy @Api)
    (runAppAsHandler env)
    (toServant httpServer)

httpApp :: AppEnv -> Application
httpApp env = serve
    (Proxy @Api)
    (server env)
