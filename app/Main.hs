module Main where

import Challenges.Philosophers
import Control.Concurrent
import Control.Monad

main = do
    done <- newEmptyMVar
    forkIO (diningPhilosophersRec done)
    takeMVar done
    print "All done"
