{-# LANGUAGE RecordWildCards #-}

module Challenges.Mentoring.DBClient where

import Challenges.Mentoring.DBPool
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString
import Data.Foldable (for_)
import GHC.Natural (Natural)

type Job = Connection ByteString -> IO ()

workerMain :: TVar Natural -> TQueue Job -> IO ()
workerMain inProgressJobs queue = do
  connPool <- initConnPool $ pure ("" :: ByteString)
  forever $ do
    job <- atomically $ do
      modifyTVar inProgressJobs (+ 1)
      readTQueue queue
    forkIO $ do 
      withConn connPool job
      atomically $ modifyTVar inProgressJobs (subtract 1)

populateQueue :: TQueue Job -> IO ()
populateQueue q = for_ [0 .. 9] \n -> do
  threadDelay 100_000
  atomically $
    writeTQueue
      q
      ( \Connection{..} -> do
          threadDelay (1_000_000 * (n + 1))
          print $ "Connection #: " <> label <> ", Queue Index: " <> show n
      )
