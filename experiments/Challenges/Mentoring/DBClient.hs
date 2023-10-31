{-# LANGUAGE RecordWildCards #-}

module Challenges.Mentoring.DBClient where

import Challenges.Mentoring.DBPool
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString
import Data.Foldable (for_)
import GHC.Natural (Natural)
import Data.Void
import Control.Exception

type Job = Connection ByteString -> IO ()

workerMain :: TVar Natural -> TQueue Job -> IO Void
workerMain inProgressJobs queue = do
  connPool <- initConnPool $ pure ("" :: ByteString)
  forever $ do
    job <- atomically $ do
      modifyTVar inProgressJobs (+ 1)
      readTQueue queue
    forkIO $ do 
      -- What happens if there is an exception
      withCounter inProgressJobs $ \j -> 
        withConn connPool \x -> do
          job x
          when (j == 9) $
            throwIO $ userError "Failed on 9"

populateQueue :: TQueue Job -> IO ()
populateQueue q = for_ [0 .. 9] \n -> do
  threadDelay 100_000
  atomically $
    writeTQueue
      q
      ( \Connection{..} -> do
          when (n == 6) $
            throwIO $ userError "Failed on 6"
          threadDelay (1_000_000 * (n + 1))
          when (n == 3) $
            throwIO $ userError "Failed on 3"
          print $ "Connection #: " <> label <> ", Queue Index: " <> show n
      )

withCounter :: TVar Natural -> (Natural -> IO ()) -> IO ()
withCounter inProgressJobs op = do
  bracket 
    (readTVarIO inProgressJobs)
    (\_ -> atomically $ modifyTVar inProgressJobs (subtract 1))
    op
    
  
