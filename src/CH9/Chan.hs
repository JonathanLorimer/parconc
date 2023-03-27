module CH9.Chan where

import CH7.Chan
import Control.Concurrent hiding (Chan)
import Control.Exception

safeReadChan :: Chan a -> IO a
safeReadChan (Chan { readVar }) = do
  modifyMVar readVar \stream -> do
    Item val t <- readMVar stream
    pure (t, val)

safeWriteChan :: Chan a -> a -> IO ()
safeWriteChan (Chan _ writeVar) val = do
  newHole <- newEmptyMVar
  mask_ $ do
    oldHole <- takeMVar writeVar
    putMVar oldHole (Item val newHole)
    putMVar writeVar newHole
