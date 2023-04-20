module Main where

import Challenges.Philosophers
import Control.Concurrent
import Control.Monad

main = do
    done <- newEmptyMVar
    forkIO (diningPhilosophers done)
    takeMVar done
    print "All done"
