{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Lib where

import Lib.Core.AppMonad (AppEnv, Env(..))
import Lib.HttpServer (httpApp)
import Network.Wai.Handler.Warp (run)
import Network.WebSockets (defaultConnectionOptions)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Lib.WSServer (wsApp)
import Lib.Core.Stats (EnvStats(..))
import Data.IORef (newIORef)

-- Setup a fresh app state
mkAppEnv :: IO AppEnv
mkAppEnv = do
  newbutplug <- newIORef 0
  let envStats = EnvStats { requestCount = newbutplug }
  pure Env{..}

runServer :: AppEnv -> IO ()
runServer env = do
  let port = 8080
  putStrLn $ "Listening on port " ++ show port
  run port $
    websocketsOr
      defaultConnectionOptions
      (wsApp env)
      (httpApp env)

main :: IO ()
main = mkAppEnv >>= runServer
