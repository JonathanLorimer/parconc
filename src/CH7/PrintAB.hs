module CH7.PrintAB where

import Control.Concurrent
import Control.Monad
import System.IO (hSetBuffering, stdout, BufferMode(..))

printAB :: IO ()
printAB = do
  hSetBuffering stdout NoBuffering
  void $ forkIO (replicateM_ 100000 (putChar 'A'))
  replicateM_ 100000 (putChar 'B')

