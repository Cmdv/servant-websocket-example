module Lib.Core.Stats
       ( EnvStats (..)
       , SuccRequests
       ) where

import Control.Concurrent (MVar)

-- | state for our stats
type SuccRequests = Int

newtype EnvStats =  EnvStats
  { succRequests :: MVar SuccRequests }
