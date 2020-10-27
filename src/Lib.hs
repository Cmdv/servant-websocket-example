{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Lib where

import Data.IORef (newIORef)
import Lib.Core.AppMonad (AppEnv, Env(..))
import Lib.Core.Stats (EnvStats(..))
import Lib.HttpServer (httpApp)
import Lib.WSServer (wsApp)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import Control.Concurrent.MVar (newMVar)

-- Setup a fresh app state
mkAppEnv :: IO AppEnv
mkAppEnv = do
  newref <- newMVar 0
  let envStats = EnvStats { requestCount = newref }
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
