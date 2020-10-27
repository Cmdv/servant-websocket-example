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
import Control.Concurrent.MVar (putMVar, modifyMVar_, readMVar)

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

-- | functions to get manipulate MVar inside EnvStats
getStatsImpl :: WithStats r m => m RequestCount
getStatsImpl = do
    statsMVar <- grab @EnvStats
    succR <- liftIO $ readMVar (requestCount statsMVar)
    pure succR

putStatsImpl :: WithStats r m => RequestCount -> m ()
putStatsImpl succR = do
    envStats <- grab @EnvStats
    liftIO $ modifyMVar_ (requestCount envStats) $ const (pure succR)

resetStatsImpl :: WithStats r m => m ()
resetStatsImpl = do
    statsMVar <- grab @EnvStats
    liftIO $ putMVar (requestCount statsMVar) 0
