module CH7.Chan where

import Control.Concurrent
    ( newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, MVar )

data Stream a = Item a (MVar (Stream a))

data Chan a = Chan
  { readVar :: MVar (MVar (Stream a)),
    writeVar :: MVar (MVar (Stream a))
  }

newChan :: IO (Chan a)
newChan = do
  hole <- newEmptyMVar
  readVar <- newMVar hole
  writeVar <- newMVar hole
  return (Chan { readVar, writeVar })

writeChan :: Chan a -> a -> IO ()
writeChan (Chan {writeVar}) val = do
  newHole <- newEmptyMVar
  oldHole <- takeMVar writeVar
  putMVar oldHole (Item val newHole)
  putMVar writeVar newHole

readChan :: Chan a -> IO a
readChan (Chan {readVar}) = do
  stream <- takeMVar readVar
  Item val t <- readMVar stream
  putMVar readVar t
  return val

badReadChan :: Chan a -> IO a
badReadChan (Chan {readVar}) = do
  stream <- takeMVar readVar
  Item val t <- takeMVar stream
  putMVar readVar t
  return val

dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeVar) = do
  hole <- readMVar writeVar
  newReadVar <- newMVar hole
  return (Chan newReadVar writeVar)

unGetChan :: Chan a -> a -> IO ()
unGetChan (Chan {readVar}) val = do
  newReadEnd <- newEmptyMVar
  readEnd <- takeMVar readVar
  putMVar newReadEnd (Item val readEnd)
  putMVar readVar newReadEnd

dupChanBlockExample :: IO ()
dupChanBlockExample = do
  c1 <- newChan @String
  c2 <- dupChan c1
  writeChan c1 "hello"
  writeChan c1 "glorious"
  writeChan c1 "world"

  badReadChan c1 >>= print
  badReadChan c1 >>= print
  badReadChan c2 >>= print

  badReadChan c1 >>= print
  badReadChan c2 >>= print


