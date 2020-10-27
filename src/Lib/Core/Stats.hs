module Lib.Core.Stats
       ( EnvStats (..)
       , RequestCount
       ) where

import Control.Concurrent.MVar (MVar)

-- | state for our stats
type RequestCount = Int

newtype EnvStats =  EnvStats
  { requestCount :: MVar RequestCount }
