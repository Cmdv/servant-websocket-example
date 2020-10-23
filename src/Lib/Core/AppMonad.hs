{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib.Core.AppMonad
       ( App (..)
       , AppEnv
       , AppServer
       , Env (..)
       , Has (..)
       , ToApi
       , runApp
       , runAppAsHandler
       , grab
       ) where

import Control.Monad.Reader.Class (MonadReader)
import Data.Kind (Type)
import Control.Monad.Reader (ReaderT, runReaderT, liftIO)
import Control.Monad.State (MonadIO)
import Control.Monad.Reader.Class (asks)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (AsServerT)
import Lib.Core.Stats (EnvStats)
import Servant.Server.Internal.Handler (Handler)

type AppServer = AsServerT App

type ToApi (site :: Type -> Type) = ToServantApi site

-- | 'Env' data type parameterized by 'App' monad
type AppEnv = Env App

data Env (m :: Type -> Type) = Env
    { envStats :: !EnvStats
    }

-- | Main application monad.
newtype App a = App
    { unApp :: ReaderT AppEnv IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env app = do
  liftIO $ runApp env app

runApp :: AppEnv -> App a -> IO a
runApp env = flip runReaderT env . unApp

{- | General type class representing which @field@ is in @env@.
can use 'Has' type class like this:

foo = do
    secret <- grab @JwtSecret
 -- secret <- asks $ obtain @JwtSecret
-}
class Has field env where
    obtain :: env -> field

-- extra instances would needed when expanding 'Env' type
instance Has EnvStats (Env m) where
  obtain = envStats

-- using type application to simplify the use of 'ask' and allow it's us
-- if there is future expanssion to 'Env' type.
grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
