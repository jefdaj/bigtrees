module System.Directory.BigTrees.Logging
  ( traceV
  )
  where

import Debug.Trace (trace)

-- TODO replace with better logging
traceV :: Bool -> String -> b -> b
traceV verbose msg b = if verbose then trace msg b else b
