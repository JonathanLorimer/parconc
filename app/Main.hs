module Main where

import Challenges.Mentoring.DBClient
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Exit (exitSuccess)

main :: IO ()
main = do
  queue <- newTQueueIO
  inProgressJobs <- newTVarIO 0
  void $ forkIO $ void (workerMain inProgressJobs queue)
  populateQueue queue
  forever $ do
    (jobs, isEmpty) <-
      atomically $
        liftA2
          (,)
          (readTVar inProgressJobs)
          (isEmptyTQueue queue)
    when (isEmpty && jobs == 0) exitSuccess
