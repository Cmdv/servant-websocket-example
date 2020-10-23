module Lib.Effect.Stats
       ( MonadStats (..)
       , getStatsImpl
       , putStatsImpl
       , resetStatsImpl
       ) where

import Prelude

import Lib.Core.Stats (RequestCount, EnvStats(..))
import Control.Monad.Reader (MonadIO, MonadReader, liftIO)
import Lib.Core.AppMonad (grab, Has, App)
import Data.IORef (readIORef, writeIORef)

-- | Describes a monad that provide an interface for a 'EnvStats' type
class Monad m => MonadStats m where
    getStats     :: m RequestCount
    putStats     :: RequestCount -> m ()
    resetStats   :: m ()

instance MonadStats App where
    getStats      = getStatsImpl
    putStats      = putStatsImpl
    resetStats    = resetStatsImpl

type WithStats r m = (MonadReader r m, Has EnvStats r, MonadIO m)

-- | functions to get manipulate IORef inside EnvStats
getStatsImpl :: WithStats r m => m RequestCount
getStatsImpl = do
    statsIORef <- grab @EnvStats
    succR <- liftIO $ readIORef (requestCount statsIORef)
    pure succR

putStatsImpl :: WithStats r m => RequestCount -> m ()
putStatsImpl succR = do
    statsIORef <- grab @EnvStats
    liftIO $ writeIORef (requestCount statsIORef) succR

resetStatsImpl :: WithStats r m => m ()
resetStatsImpl = do
    statsIORef <- grab @EnvStats
    liftIO $ writeIORef (requestCount statsIORef) 0
