module Challenges.Philosophers where

import Control.Concurrent
import Control.Monad (forever)
import Control.Exception
import System.Exit

data Fork = Fork

data Philosopher
  = Thinking
  | Eating Fork Fork

newtype PhilosopherDied = PhilosopherDied String deriving Show

instance Exception PhilosopherDied

runPhilosopher :: Chan String -> (MVar Philosopher, String) -> (MVar Fork, String) -> (MVar Fork, String) -> IO ()
runPhilosopher chan (pState, pName) (f1State, f1Name) (f2State, f2Name) = do
  tid <- myThreadId
  forever $
    takeMVar pState >>= \case
      Thinking -> do
        bracket (forkIO do
                    threadDelay (10 * (10 ^ 6))
                    throwTo tid (PhilosopherDied pName))
                (`throwTo` ThreadKilled)
                (\_ -> do
                    f1 <- takeMVar f1State
                    writeChan chan (pName ++ " took " ++ f1Name)
                    f2 <- takeMVar f2State
                    writeChan chan (pName ++ " took " ++ f2Name)
                    putMVar pState (Eating f1 f2))
      Eating f1 f2 -> do
        writeChan chan (pName ++ " is eating")
        threadDelay (1 * (10 ^ 6))
        writeChan chan (pName ++ " is done eating")
        putMVar f1State f1
        writeChan chan (pName ++ " put " ++ f1Name ++ " back")
        putMVar f2State f2
        writeChan chan (pName ++ " put " ++ f2Name ++ " back")
        putMVar pState Thinking
        writeChan chan (pName ++ " is thinking")

diningPhilosophers :: MVar () -> IO ()
diningPhilosophers done = do
  tId <- myThreadId
  chan <- newChan

  fork1 <- newMVar Fork
  fork2 <- newMVar Fork
  fork3 <- newMVar Fork
  fork4 <- newMVar Fork
  fork5 <- newMVar Fork

  plato <- newMVar Thinking
  konfuzius <- newMVar Thinking
  socrates <- newMVar Thinking
  voltaire <- newMVar Thinking
  descartes <- newMVar Thinking

  let forkPhilosopher = flip forkFinally (handlePhilosopherDeath tId)

  forkPhilosopher $ runPhilosopher chan (plato, "plato") (fork1, "fork1") (fork2, "fork2")
  forkPhilosopher $ runPhilosopher chan (konfuzius, "konfuzius") (fork2, "fork2") (fork3, "fork3")
  forkPhilosopher $ runPhilosopher chan (socrates, "socrates") (fork3, "fork3") (fork4, "fork4")
  forkPhilosopher $ runPhilosopher chan (voltaire, "voltaire") (fork4, "fork4") (fork5, "fork5")
  forkPhilosopher $ runPhilosopher chan (descartes, "descartes") (fork5, "fork5") (fork1, "fork1")

  forkIO $ forever (readChan chan >>= print)

  pure ()
    where
      handlePhilosopherDeath :: ThreadId -> Either SomeException () -> IO ()
      handlePhilosopherDeath tId (Left e) =
        case fromException e of
          (Just (PhilosopherDied philName)) -> do
            print (philName ++ " has died ")
            putMVar done ()
            killThread tId
          _ -> pure ()
      handlePhilosopherDeath _ _ = pure ()

-- Can do it with 1 less mvar per philosopher and recursion
-- if we don't need the philosophers to kill their own thread to simulate starving

runPhilosopherRec :: Chan String -> (Philosopher, String) -> (MVar Fork, String) -> (MVar Fork, String) -> IO ()
runPhilosopherRec chan (Thinking, pName) fork1@(f1State, f1Name) fork2@(f2State, f2Name) = do
  f1 <- takeMVar f1State
  writeChan chan (pName ++ " took " ++ f1Name)
  f2 <- takeMVar f2State
  writeChan chan (pName ++ " took " ++ f2Name)
  runPhilosopherRec chan (Eating f1 f2, pName) fork1 fork2
runPhilosopherRec chan (Eating f1 f2, pName) fork1@(f1State, f1Name) fork2@(f2State, f2Name) = do
  writeChan chan (pName ++ " is eating")
  threadDelay (1 * (10 ^ 6))
  writeChan chan (pName ++ " is done eating")
  putMVar f1State f1
  writeChan chan (pName ++ " put " ++ f1Name ++ " back")
  putMVar f2State f2
  writeChan chan (pName ++ " put " ++ f2Name ++ " back")
  runPhilosopherRec chan (Thinking, pName) fork1 fork2

diningPhilosophersRec :: MVar () -> IO ()
diningPhilosophersRec done = do
  tId <- myThreadId
  chan <- newChan

  fork1 <- newMVar Fork
  fork2 <- newMVar Fork
  fork3 <- newMVar Fork
  fork4 <- newMVar Fork
  fork5 <- newMVar Fork

  let forkPhilosopher = flip forkFinally (handlePhilosopherDeath tId)

  forkPhilosopher $ runPhilosopherRec chan (Thinking, "plato") (fork1, "fork1") (fork2, "fork2")
  forkPhilosopher $ runPhilosopherRec chan (Thinking, "konfuzius") (fork2, "fork2") (fork3, "fork3")
  forkPhilosopher $ runPhilosopherRec chan (Thinking, "socrates") (fork3, "fork3") (fork4, "fork4")
  forkPhilosopher $ runPhilosopherRec chan (Thinking, "voltaire") (fork4, "fork4") (fork5, "fork5")
  forkPhilosopher $ runPhilosopherRec chan (Thinking, "descartes") (fork5, "fork5") (fork1, "fork1")

  forkIO $ forever (readChan chan >>= print)

  pure ()
    where
      handlePhilosopherDeath :: ThreadId -> Either SomeException () -> IO ()
      handlePhilosopherDeath tId (Left e) =
        case fromException e of
          (Just (PhilosopherDied philName)) -> do
            print (philName ++ " has died ")
            putMVar done ()
            killThread tId
          _ -> pure ()
      handlePhilosopherDeath _ _ = pure ()
