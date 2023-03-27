module CH8.AsyncAwait where

import Control.Concurrent
import Network.HTTP.Client
import Control.Monad
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client.TLS
import System.TimeIt

newtype Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  void $ forkIO (action >>= putMVar var)
  return (Async var)

await :: Async a -> IO a
await (Async var) = readMVar var

runReq :: Manager -> String -> IO ByteString
runReq man url = timeItNamed url $ parseRequest url >>= \req -> responseBody <$> httpLbs req man

getURLsAsync :: IO ()
getURLsAsync = do
  manager <- newManager tlsManagerSettings
  let req = runReq manager

  shovelReq <- async $ req "http://www.wikipedia.org/wiki/Shovel"
  spadeReq <- async $ req "http://www.wikipedia.org/wiki/Spade"

  shovelRes <- await shovelReq
  spadeRes <- await spadeReq

  print $ BS.length shovelRes
  print $ BS.length spadeRes


sites :: [String]
sites = ["http://www.google.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.com/wiki/Spade",
         "http://www.wikipedia.com/wiki/Shovel"]

getURLsAsyncMap :: IO ()
getURLsAsyncMap = do
  manager <- newManager tlsManagerSettings
  let req = runReq manager

  as <- mapM (async . req) sites
  mapM_ await as

getURLsAsyncFail :: IO ()
getURLsAsyncFail = do
  manager <- newManager tlsManagerSettings
  let req = runReq manager

  as <- mapM (async . req) $ "http://www.fhadlfaaalkahdlfjahdsflha.com" : sites
  mapM_ await as
