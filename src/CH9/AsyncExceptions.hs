module CH9.AsyncExceptions where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.IO
import Text.Printf
import Data.Either

data Async a = Async ThreadId (MVar (Either SomeException a))

cancel :: Async a -> IO ()
cancel (Async t _) = throwTo t ThreadKilled

awaitCatch :: Async a -> IO (Either SomeException a)
awaitCatch (Async _ var) = readMVar var

async :: IO a -> IO (Async a)
async action = do
  m <- newEmptyMVar
  t <- forkIO $ try action >>= putMVar m
  return (Async t m)

runReq :: Manager -> String -> IO (String, ByteString)
runReq man url = fmap (url,) $ parseRequest url >>= \req -> responseBody <$> httpLbs req man

sites :: [String]
sites =
  [ "http://www.google.com",
    "http://www.bing.com",
    "http://www.yahoo.com",
    "http://www.wikipedia.com/wiki/Spade",
    "http://www.wikipedia.com/wiki/Shovel"
  ]

getURLsAsyncCancellable :: IO ()
getURLsAsyncCancellable = do
  manager <- newManager tlsManagerSettings
  let req = runReq manager

  as <- mapM (async . req) sites

  bracket
    (forkIO $ do
      hSetBuffering stdin NoBuffering
      forever $ do
        c <- getChar
        when (c == 'q') $ putStrLn "\ncancelling requests" >> mapM_ cancel as)
    killThread
    (\_ -> do
      rs <- mapM awaitCatch as
      print $ fmap (fmap fst) rs
      printf "%d/%d succeeded\n" (length (rights rs)) (length rs))
