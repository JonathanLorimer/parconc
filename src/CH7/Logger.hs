module CH7.Logger where

import Control.Concurrent
import Control.Monad

newtype Logger = Logger (MVar LogCommand)

data LogCommand = Message String | Stop (MVar ())

logger :: Logger -> IO ()
logger (Logger m) = loop
 where
  loop = do
    cmd <- takeMVar m
    case cmd of
      Message msg -> do
        putStrLn msg
        loop
      Stop s -> do
        putStrLn "logger: stop"
        putMVar s ()

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  void . forkIO . logger $ l
  return l

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

logMain :: IO ()
logMain = do
  l <- initLogger
  logMessage l "hello"
  logMessage l "goodbye"
  logStop l
