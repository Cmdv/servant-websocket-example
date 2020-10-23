module Lib.HttpServer.Env
       ( -- AppEnv
       --, Env (..)
       ) where

import Data.Kind (Type)
import Lib.Core.Stats (EnvStats)
import Lib.Core.AppMonad (App)

-- | 'Env' data type parameterized by 'App' monad
-- type AppEnv = Env App

-- data Env (m :: Type -> Type) = Env
--     { envStats :: !EnvStats
--     }
