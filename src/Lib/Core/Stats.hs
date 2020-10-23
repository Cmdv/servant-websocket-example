module Lib.Core.Stats
       ( EnvStats (..)
       , RequestCount
       ) where

import Data.IORef (IORef)

-- | state for our stats
type RequestCount = Int

newtype EnvStats =  EnvStats
  { requestCount :: IORef RequestCount }
