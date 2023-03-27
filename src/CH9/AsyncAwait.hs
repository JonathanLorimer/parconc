module CH9.AsyncAwait where

import Control.Concurrent
    ( ThreadId, forkIO, newEmptyMVar, putMVar )
import Control.Exception
import CH9.AsyncExceptions

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action fun =
  mask $ \restore ->
    forkIO (do r <- try (restore action); fun r)

async :: IO a -> IO (Async a)
async action = do
   m <- newEmptyMVar
   t <- forkFinally action (putMVar m)
   return (Async t m)
