{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Lib where

import Lib.Core.AppMonad (AppEnv, Env(..))
import Lib.HttpServer.Server (httpApp)
import Network.Wai.Handler.Warp (run)
import Network.WebSockets (defaultConnectionOptions)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Lib.WSServer (wsApp)
import Control.Concurrent (newEmptyMVar)
import Lib.Core.Stats (EnvStats(..))

-- Setup a fresh app state
mkAppEnv :: IO AppEnv
mkAppEnv = do
  emptyMVar <- newEmptyMVar
  let envStats = EnvStats { succRequests = emptyMVar }
  pure Env{..}

runServer :: AppEnv -> IO ()
runServer env = do
    run 8080 $
      websocketsOr
        defaultConnectionOptions
        (wsApp env)
        (httpApp env)

main :: IO ()
main = mkAppEnv >>= runServer
