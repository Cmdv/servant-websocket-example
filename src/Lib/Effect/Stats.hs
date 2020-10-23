module Lib.Effect.Stats
       ( MonadStats (..)
       , getStatsImpl
       , putStatsImpl
       , resetStatsImpl
       ) where

import Prelude

import Lib.Core.Stats (SuccRequests, EnvStats(..))
import Control.Monad.Reader (MonadIO, MonadReader, liftIO)
import Control.Concurrent (putMVar, readMVar)
import Lib.Core.AppMonad (grab, Has, App)

-- | Describes a monad that provide an interface for a 'EnvStats' type
class Monad m => MonadStats m where
    getStats     :: m SuccRequests
    putStats     :: SuccRequests -> m ()
    resetStats   :: m ()

instance MonadStats App where
    getStats       = getStatsImpl
    putStats       = putStatsImpl
    resetStats    = resetStatsImpl

type WithStats r m = (MonadReader r m, Has EnvStats r, MonadIO m)

-- | functions to get manipulate MVar inside EnvStats
getStatsImpl :: WithStats r m => m SuccRequests
getStatsImpl = do
    statsMvar <- grab @EnvStats
    succR <- liftIO $ readMVar (succRequests statsMvar)
    pure succR

putStatsImpl :: WithStats r m => SuccRequests -> m ()
putStatsImpl succR = do
    statsMvar <- grab @EnvStats
    liftIO $ putMVar (succRequests statsMvar) succR

resetStatsImpl :: WithStats r m => m ()
resetStatsImpl = do
    statsMvar <- grab @EnvStats
    liftIO $ putMVar (succRequests statsMvar) 0
