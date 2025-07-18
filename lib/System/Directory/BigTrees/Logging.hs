{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module System.Directory.BigTrees.Logging
  -- ( traceV
  -- , LogLevel (..)
  -- , LogContext
  -- , LogFn
  -- , initLogger
  -- , log
  -- , die
  -- , logMaybe
  -- , logMaybeUnsafe
  -- , incLogProgressST
  -- )
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
import Control.DeepSeq (deepseq)
import System.IO (stderr, hPutStrLn, hFlush)

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

initLogger :: IO (LogFn, IO ())
initLogger = do
  -- Microseconds might be useful here for ordering, but sadly Data.UnixTime
  -- ignores them. Maybe that's good for efficiency?
  timeCache <- newTimeCache "%Y-%m-%d %H:%M:%S"
  (logger, cleanupLogger) <- newTimedFastLogger timeCache (LogStderr defaultBufSize)
  return (log logger, cleanupLogger)

-- log :: ToLogStr a => TimedFastLogger -> LogFn a
log :: TimedFastLogger -> LogFn
log logger level context msg = logger $ \ft -> toLogStr (logLine level context msg ft) <> "\n"

initLogger2 = do
  timeCache :: IO FormattedTime <- newTimeCache "%Y-%m-%d %H:%M:%S"
  loggerSet :: LoggerSet <- newStderrLoggerSet defaultBufSize
  return (loggerSet, timeCache)

-- initLogger3 :: IO (LogFn, IO ())
initLogger3 = do
  -- Microseconds might be useful here for ordering, but sadly Data.UnixTime
  -- ignores them. Maybe that's good for efficiency?
  timeCache <- newTimeCache "%Y-%m-%d %H:%M:%S"
  (logger, cleanupLogger) <- newTimedFastLogger timeCache (LogStderr defaultBufSize)
  return (logger, cleanupLogger)

-- attempt at getting flushing to work properly in die,
-- and then if so to add back the timestamp? or both at once
log2 lSet level context msg date = do
  -- let date = B8.pack $ "XXXX-XX-XX XX:XX:XX" -- TODO how to get date here?
  let lStr = logLine level context msg date
  pushLogStrLn lSet lStr
  flushLogStr lSet

-- works! just needs better UX and probably to adjust all the Maybe LogFn types
testLogger2 :: IO ()
testLogger2 = do
  (loggerSet, timeCache) <- initLogger2
  log2 loggerSet DebugL   "testLogger2" "this is a test" =<< timeCache
  log2 loggerSet InfoL    "testLogger2" "this is a test" =<< timeCache
  log2 loggerSet WarningL "testLogger2" "this is a test" =<< timeCache
  log2 loggerSet ErrorL   "testLogger2" "this is a test" =<< timeCache
  error "does it flush first?"

testLogger3 :: IO ()
testLogger3 = do
  (logger :: TimedFastLogger, cleanupLogger) <- initLogger3
  return ()

logLine :: LogLevel -> LogContext -> B8.ByteString -> FormattedTime -> LogStr
logLine level context msg timestamp =
  let sep = toLogStr (" | " :: String)
  in mconcat $ L.intersperse sep
       [ toLogStr timestamp
       , toLogStr level
       , toLogStr context
       , toLogStr msg
       ]

-- TODO see if passing the cleanup fn via config solves this not printing
-- die :: Maybe LogFn -> LogContext -> B8.ByteString -> a
-- die mLog context msg =
--   case mLog of
--     Nothing -> error msg'
--     Just fn -> let msg'' = unsafePerformIO (fn ErrorL context msg >> hFlush stderr >> return msg')
--                in error msg''
--   where
--     msg' = B8.unpack msg

-- Crash the program, making sure to log the error properly first
die :: Maybe LogFn -> LogContext -> B8.ByteString -> a
die mLog context msg =
  let date  = B8.pack $ "XXXX-XX-XX XX:XX:XX" -- TODO how to get date here?
      line  = logLine ErrorL context msg date
      line' = B8.unpack $ fromLogStr line
      line'' = drop 23 $ show line'
  in case mLog of
       Nothing -> error line''
       Just _  -> error $ unsafePerformIO $ hPutStrLn stderr line' >> hFlush stderr >> return line''

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
