module CH11.Async where

import Control.Concurrent.STM
import Control.Exception
import Control.Concurrent
import Network.HTTP.Client
import Control.Monad
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client.TLS

data Async a = Async ThreadId (STM (Either SomeException a))

instance Functor Async where
  fmap f (Async t stm) = Async t (fmap (fmap f) stm)

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyTMVarIO
  t <- forkFinally action (atomically . putTMVar var)
  pure (Async t (readTMVar var))

withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync io op = bracket (async io) cancel op

runReq :: Manager -> String -> IO ByteString
runReq man url = parseRequest url >>= \req -> responseBody <$> httpLbs req man

getUrls :: IO ()
getUrls = do
  manager <- newManager tlsManagerSettings
  withAsync (runReq manager "http://www.wikipedia.org/wiki/Shovel") $ \a1 ->
    withAsync (runReq manager "http://www.wikipedia.org/wiki/Spade") $ \a2 -> do
      r1 <- wait a1
      r2 <- wait a2
      print (BL.length r1, BL.length r2)

waitBoth :: Async a -> Async b -> IO (a, b)
waitBoth a1 a2 =
  atomically $ do
    r1 <- waitSTM a1 `orElse` (do waitSTM a2; retry)
    r2 <- waitSTM a2
    pure (r1, r2)

concurrently :: IO a -> IO b -> IO (a,b)
concurrently ioa iob =
  withAsync ioa $ \a ->
  withAsync iob $ \b ->
    waitBoth a b

getUrlsConcurrently :: IO ()
getUrlsConcurrently = do
  manager <- newManager tlsManagerSettings
  (r1, r2) <- concurrently
                (runReq manager "http://www.wikipedia.org/wiki/Shovel")
                (runReq manager "http://www.wikipedia.org/wiki/Spade")
  print (BL.length r1, BL.length r2)

race :: IO a -> IO b -> IO (Either a b)
race ioa iob =
  withAsync ioa $ \a ->
  withAsync iob $ \b ->
    waitEither a b
