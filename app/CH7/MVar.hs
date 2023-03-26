module MVar where

import Control.Concurrent
import Control.Monad

simple :: IO ()
simple = do
  m <- newEmptyMVar
  void $ forkIO do
    threadDelay (10 ^ 6 * 2)
    putMVar m 'x'
  r <- print "waiting for mvar" >> takeMVar m
  print r

simple2 :: IO ()
simple2 = do
  m <- newEmptyMVar
  forkIO $ putMVar m 'x'
  forkIO $ putMVar m 'y'
  forkIO $ putMVar m 'z'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r
