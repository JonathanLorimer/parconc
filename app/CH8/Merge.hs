module CH8.Merge where

import Control.Concurrent
import Network.HTTP.Client
import Control.Monad
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client.TLS
import Text.Printf
import Control.Exception
import CH8.AsyncAwaitExcept (Async(..), await, async)

runReq :: Manager -> String -> IO (String, ByteString)
runReq man url = fmap (url,) $ parseRequest url >>= \req -> responseBody <$> httpLbs req man

runReqVar :: Manager -> MVar (String, ByteString) -> String -> IO ByteString
runReqVar man var url = do
  (_, res) <- runReq man url
  putMVar var (url, res)
  pure res

sites :: [String]
sites = ["http://www.google.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.com/wiki/Spade",
         "http://www.wikipedia.com/wiki/Shovel"]

getURLsRace :: IO ()
getURLsRace = do
  manager <- newManager tlsManagerSettings
  winnerVar <- newEmptyMVar
  let req = runReqVar manager winnerVar

  mapM_ (forkIO . void . req) sites

  (url, response) <- takeMVar winnerVar
  printf "%s was first (%d bytes)\n" url (BS.length response)
  replicateM_ (length sites - 1) (takeMVar winnerVar)

--------------------------------------------------------------------

awaitEither :: Async a -> Async b -> IO (Either a b)
awaitEither a b = do
  m <- newEmptyMVar
  void . forkIO $ do r <- try (fmap Left  (await a)); putMVar m r
  void . forkIO $ do r <- try (fmap Right (await b)); putMVar m r
  await (Async m)

awaitFirst :: [Async a] -> IO a
awaitFirst as = do
  winnerVar <- newEmptyMVar
  let forkwait a = forkIO $ try (await a) >>= putMVar winnerVar
  mapM_ forkwait as
  await (Async winnerVar)

getURLsRaceAsync :: IO ()
getURLsRaceAsync = do
  manager <- newManager tlsManagerSettings
  let req = runReq manager

  as <- mapM (async . req) sites

  (url, response) <- awaitFirst as
  printf "%s was first (%d bytes)\n" url (BS.length response)
  mapM_ await as
