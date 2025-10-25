{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Directory.BigTrees.Logging
  ( LogLevel (..)
  , LogContext
  , LogCfg (..)
  , initLogger
  , cleanupLogger
  , addLogContext
  , log
  , die
  , logUnsafe
  , incLogProgressST
  )
  where

import Prelude hiding (log)
import Debug.Trace (trace)
import System.Log.FastLogger
import System.Log.FastLogger.LoggerSet (rmLoggerSet)
import qualified Data.List as L
import Data.Char (toUpper)
import qualified Data.ByteString.Char8 as B8
import System.IO.Unsafe (unsafePerformIO)
import Data.STRef (STRef(..), newSTRef, readSTRef, writeSTRef)
import Control.Monad (when)
import Control.Monad.ST.Strict (ST)
import Control.DeepSeq (deepseq)
import System.IO (stderr, hPutStrLn, hFlush)

type LogContext = String

data LogLevel = DebugL | InfoL | WarningL | ErrorL
  deriving stock (Read, Show, Eq, Ord)

instance ToLogStr LogLevel where
  toLogStr = toLogStr . map toUpper . init . show

data LogCfg = NoLog | LogCfg
  { lcContext :: LogContext       -- ^ a string like "main.mymodule.mycmd"
  , lcLevel   :: LogLevel         -- ^ minimum level that will be logged
  , lcLogger  :: LoggerSet        -- ^ fast-logger LoggerSet for stderr
  , lcTime    :: IO FormattedTime -- ^ fast-logger time cache
  }

addLogContext :: LogCfg -> String -> LogCfg
addLogContext NoLog _ = NoLog
addLogContext cfg@(LogCfg {}) ctx = cfg { lcContext = lcContext cfg ++ "." ++ ctx }

-- TODO setLogLevel?

initLogger :: String -> LogLevel -> IO LogCfg
initLogger initialContext minLogLevel = do
  loggerSet <- newStderrLoggerSet defaultBufSize
  timeCache <- newTimeCache "%Y-%m-%d %H:%M:%S"
  return $ LogCfg
    { lcContext = initialContext
    , lcLevel   = minLogLevel
    , lcLogger  = loggerSet
    , lcTime    = timeCache
    }

cleanupLogger :: LogCfg -> IO ()
cleanupLogger NoLog = return () 
cleanupLogger cfg = rmLoggerSet $ lcLogger cfg

log :: LogCfg -> LogLevel -> B8.ByteString -> IO ()
log NoLog _ _ = return ()
log (LogCfg {..}) level msg = when (level >= lcLevel) $ do
  time <- lcTime
  let lStr = formatLogLine level lcContext msg time
  pushLogStrLn lcLogger lStr

-- log an error and then crash the program
-- TODO can this stack overflow?
die :: LogCfg -> B8.ByteString -> a
die NoLog msg = error $ B8.unpack $ "ERROR: " <> msg
die cfg@(LogCfg {..}) msg =
  (unsafePerformIO $ do
    log cfg ErrorL msg
    flushLogStr lcLogger)
  `seq`
    error $ lcContext ++ " " ++ B8.unpack msg

-- TODO remove once new logging works in the main program
testLogger :: IO ()
testLogger = do
  cfg :: LogCfg <- initLogger "testLogger" DebugL
  log cfg   DebugL   "testing log with DebugL"
  log cfg   InfoL   "testing log with InfoL"
  log (addLogContext cfg "moreContext") DebugL   "testing log with DebugL"
  log NoLog DebugL   "testing log with DebugL and NoLog"
  log cfg   WarningL "testing log with WarningL"
  die cfg "testing die"
  die cfg "testing die"
  die cfg "testing die"
  die cfg "testing die"
  log cfg   ErrorL   "testing log with ErrorL"
  return ()

formatLogLine :: LogLevel -> LogContext -> B8.ByteString -> FormattedTime -> LogStr
formatLogLine level context msg timestamp =
  let sep = toLogStr (" | " :: String)
  in mconcat $ L.intersperse sep
       [ toLogStr timestamp
       , toLogStr level
       , toLogStr context
       , toLogStr msg
       ]

-- log from any context (pure code, ST, ...) using unsafePeformIO
logUnsafe :: LogCfg -> LogLevel -> B8.ByteString -> a -> a
logUnsafe cfg level msg rtn = unsafePerformIO (log cfg level msg) `seq` rtn

-- TODO remove, or unify with DupeMap.incAddTreeProgress
incLogProgressST :: LogCfg -> STRef s Int -> ST s ()
incLogProgressST cfg intRef = do
  n <- readSTRef intRef
  let n' = n + 1
  logUnsafe cfg InfoL ("increment stref to " <> B8.pack (show n')) $
    writeSTRef intRef n'
