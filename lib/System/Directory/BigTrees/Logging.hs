{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module System.Directory.BigTrees.Logging
  ( traceV
  , LogLevel (..)
  , LogContext
  , LogFn
  , createStderrLogger
  , log
  , logMaybe
  , logMaybeUnsafe
  , incLogProgressST
  )
  where

import Prelude hiding (log)
import Debug.Trace (trace)
import System.Log.FastLogger
import qualified Data.List as L
import Data.Char (toUpper)
import qualified Data.ByteString.Char8 as B8
import System.IO.Unsafe (unsafePerformIO)
import Data.STRef (STRef(..), newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST.Strict (ST)

-- TODO replace with better logging
traceV :: Bool -> String -> b -> b
traceV verbose msg b = if verbose then trace msg b else b

type LogContext = String

-- TODO would it make sense to use B8.ByteString rather than String here?
-- TODO use String here instead of B8.ByteString?
-- TODO or leave the original ToLogStr and specify in each module?
-- type LogFn a = ToLogStr a => LogLevel -> LogContext -> a -> IO ()
type LogFn = LogLevel -> LogContext -> B8.ByteString -> IO ()

data LogLevel = DebugL | InfoL | WarningL | ErrorL
  deriving (Read, Show)

instance ToLogStr LogLevel where
  toLogStr = toLogStr . map toUpper . init . show

createStderrLogger :: IO (TimedFastLogger, IO ())
createStderrLogger = do
  -- Microseconds might be useful here for ordering, but sadly Data.UnixTime
  -- ignores them. Maybe that's good for efficiency?
  timeCache <- newTimeCache "%Y-%m-%d %H:%M:%S"
  newTimedFastLogger timeCache (LogStderr defaultBufSize)

-- log :: ToLogStr a => TimedFastLogger -> LogFn a
log :: TimedFastLogger -> LogFn
log logger level context msg = logger $ \ft -> toLogStr (msgWithContext ft) <> "\n"
  where
    sep = toLogStr (" | " :: String)
    msgWithContext timestamp = mconcat $ L.intersperse sep
      [ toLogStr timestamp
      , toLogStr level
      , toLogStr context
      , toLogStr msg
      ]

logMaybe :: Maybe LogFn -> LogLevel -> LogContext -> B8.ByteString -> IO ()
logMaybe mLog level context msg = case mLog of
  Nothing -> return ()
  Just fn -> fn level context msg

logMaybeUnsafe :: Maybe LogFn -> LogLevel -> LogContext -> B8.ByteString -> a -> a
logMaybeUnsafe mLog level context msg rtn = case mLog of
  Nothing -> rtn
  Just fn -> unsafePerformIO (fn level context msg) `seq` rtn

-- TODO remove, or unify with DupeMap.incAddTreeProgress
incLogProgressST :: Maybe LogFn -> LogContext -> STRef s Int -> ST s ()
incLogProgressST mLog ctx intRef = do
  n <- readSTRef intRef
  let n' = n + 1
  logMaybeUnsafe mLog InfoL ctx ("increment stref to " <> B8.pack (show n')) $
    writeSTRef intRef n'
