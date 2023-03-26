module CH8.AsyncAwaitExcept where

import Control.Concurrent
import Control.Monad
import Control.Exception
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.ByteString.Lazy
import System.TimeIt

newtype Async a = Async (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  void $ forkIO (try action >>= putMVar var)
  return (Async var)

awaitCatch :: Async a -> IO (Either SomeException a)
awaitCatch (Async var) = readMVar var

await :: Async a -> IO a
await asyncAction = do
  r <- awaitCatch asyncAction
  case r of
    Left e -> throwIO e
    Right a -> pure a

runReq :: Manager -> String -> IO ByteString
runReq man url = timeItNamed url $ parseRequest url >>= \req -> responseBody <$> httpLbs req man

sites :: [String]
sites = ["http://www.dafhasdlkhfkale.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.com/wiki/Spade",
         "http://www.wikipedia.com/wiki/Shovel"]

getURLsAsyncException :: IO ()
getURLsAsyncException = do
  manager <- newManager tlsManagerSettings
  let req = runReq manager

  as <- mapM (async . req) sites
  mapM_ await as
