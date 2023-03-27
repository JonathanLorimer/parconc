module CH8.GetURLs where

import Control.Concurrent
import Network.HTTP.Client
import Control.Monad
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client.TLS

getURLs :: IO ()
getURLs = do
  manager <- newManager tlsManagerSettings

  shovelVar <- newEmptyMVar
  spadeVar <- newEmptyMVar

  void $ forkIO $ runReq manager shovelVar "http://www.wikipedia.org/wiki/Shovel"
  void $ forkIO $ runReq manager spadeVar "http://www.wikipedia.org/wiki/Spade"

  shovelRes <- takeMVar shovelVar                                     -- 5
  spadeRes <- takeMVar spadeVar

  print $ BS.length shovelRes
  print $ BS.length spadeRes

  pure ()
    where
      runReq :: Manager -> MVar ByteString -> String -> IO ()
      runReq man resVar url = do
        request <- parseRequest url
        response <- httpLbs request man
        putMVar resVar (responseBody response)
