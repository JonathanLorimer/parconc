module Main where

import Challenges.Mentoring.Core (initLogger)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (void)
import Challenges.Mentoring.Core (withLogger)

main :: IO ()
main = withLogger \putLog -> 
  void $ mapConcurrently putLog (fmap (\n -> "log number: " <> show n) [0 .. 100])
