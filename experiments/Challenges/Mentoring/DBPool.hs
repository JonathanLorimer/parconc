{-# LANGUAGE RecordWildCards #-}

module Challenges.Mentoring.DBPool where

import Control.Concurrent.STM
import Control.Exception
import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector

-- many-to-many:
--  connection pool
--  thread manager
--  ability to cancel threads (w/o leaking connections)

data ConnectionPool a = ConnectionPool
  { connections :: Vector (TVar a)
  , freeConnections :: TVar (Set Int)
  }

data Connection a = Connection
  { connState :: TVar a
  , label :: String
  , release :: IO ()
  }

-- Thread Manager Operations

withConn :: ConnectionPool a -> (Connection a -> IO ()) -> IO ()
withConn cp = bracket (acquire cp) release

-- Connection Operations

put :: Connection ByteString -> ByteString -> IO ()
put Connection{..} = atomically . writeTVar connState

get :: Connection ByteString -> IO ByteString
get Connection{..} = readTVarIO connState

acquire :: ConnectionPool a -> IO (Connection a)
acquire ConnectionPool{..} = do
  idx <- atomically do
    set <- readTVar freeConnections
    let (idx, newSet) = Set.deleteFindMin set
    writeTVar freeConnections newSet
    pure idx
  print $ "Index of this acquire is: " <> show idx
  let release = do 
        putStrLn $ "About to release: " <> show idx
        atomically do
          set <- readTVar freeConnections
          writeTVar freeConnections $ Set.insert idx set
        putStrLn $ "released: " <> show idx
  pure $ Connection (connections ! idx) (show idx)  release

introspect :: ConnectionPool a -> IO ()
introspect ConnectionPool{..} = do
  conns <- readTVarIO freeConnections
  let s1 = Set.fromList [0 .. 9]
      busy = Set.difference s1 conns
  print $ fmap (,"busy" :: String) (Set.toList busy) <> fmap (,"free" :: String) (Set.toList conns)

-- Management

initConnPool :: IO a -> IO (ConnectionPool a)
initConnPool action = do
  vars <- replicateM 10 (action >>= newTVarIO)
  freeConns <- newTVarIO $ Set.fromList [0 .. 9]
  pure $ ConnectionPool vars freeConns
