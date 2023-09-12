module Challenges.Mentoring.Core where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (newTQueueIO)
import Control.Concurrent.STM.TQueue (readTQueue, writeTQueue, isEmptyTQueue)
import Control.Monad (forever, unless)
import Control.Monad.STM (atomically)
import Data.Functor (void)

initLogger :: IO (String -> IO (), IO ())
initLogger = do
  queue <- newTQueueIO
  void . forkIO $ forever do
    threadDelay 500
    entry <- atomically $ readTQueue queue
    putStrLn entry
  let finalizeQueue = do
        isEmpty <- atomically $ isEmptyTQueue queue
        unless isEmpty finalizeQueue
  pure (atomically . writeTQueue queue, finalizeQueue)

withLogger :: ((String -> IO ()) -> IO ()) -> IO ()
withLogger action = do
  (putLog, finalize) <- initLogger 
  action putLog
  finalize
