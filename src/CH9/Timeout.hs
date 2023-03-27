module CH9.Timeout where

import Control.Concurrent
import Control.Exception
import Data.Typeable
import Data.Unique

newtype Timeout = Timeout Unique
  deriving (Typeable, Eq)

instance Show Timeout where
  show :: Timeout -> String
  show (Timeout unique) = "Timeout " <> show (hashUnique unique)

instance Exception Timeout

timeout :: Int -> IO a -> IO (Maybe a)
timeout t m
  | t < 0 = fmap Just m
  | t == 0 = return Nothing
  | otherwise = do
      pid <- myThreadId
      u <- newUnique
      let ex = Timeout u
      handleJust
        (\e -> if e == ex then Just () else Nothing)
        (\_ -> return Nothing)
        ( bracket
            ( forkIO $ do
                threadDelay t
                throwTo pid ex
            )
            (`throwTo` ThreadKilled)
            (const $ fmap Just m)
        )
