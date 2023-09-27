module Challenges.Philosophers.MVar where

import Control.Concurrent
import Control.Exception
import Control.Monad (forever, void)

data Fork = Fork

data Philosopher
  = Thinking
  | Eating Fork Fork

newtype PhilosopherDied = PhilosopherDied String deriving (Show)

instance Exception PhilosopherDied

delayMilliSecs :: Int -> Int
delayMilliSecs = (*) ((10 :: Int) ^ (6 :: Int))

-- Unfortunately this solution depends on the fairness of haskell's concurrency scheduler
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

  void $ forkPhilosopher $ runPhilosopher chan (plato, "plato") (fork1, "fork1") (fork2, "fork2")
  void $ forkPhilosopher $ runPhilosopher chan (konfuzius, "konfuzius") (fork2, "fork2") (fork3, "fork3")
  void $ forkPhilosopher $ runPhilosopher chan (socrates, "socrates") (fork3, "fork3") (fork4, "fork4")
  void $ forkPhilosopher $ runPhilosopher chan (voltaire, "voltaire") (fork4, "fork4") (fork5, "fork5")
  void $ forkPhilosopher $ runPhilosopher chan (descartes, "descartes") (fork5, "fork5") (fork1, "fork1")

  void $ forkIO $ forever (readChan chan >>= print)
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

runPhilosopher :: Chan String -> (MVar Philosopher, String) -> (MVar Fork, String) -> (MVar Fork, String) -> IO ()
runPhilosopher chan (pState, pName) (f1State, f1Name) (f2State, f2Name) = do
  tid <- myThreadId
  forever $
    takeMVar pState >>= \case
      Thinking -> do
        bracket
          ( forkIO do
              threadDelay (delayMilliSecs 10)
              throwTo tid (PhilosopherDied pName)
          )
          (`throwTo` ThreadKilled)
          ( \_ -> do
              f1 <- takeMVar f1State
              writeChan chan (pName ++ " took " ++ f1Name)
              f2 <- takeMVar f2State
              writeChan chan (pName ++ " took " ++ f2Name)
              putMVar pState (Eating f1 f2)
          )
      Eating f1 f2 -> do
        writeChan chan (pName ++ " is eating")
        threadDelay (delayMilliSecs 1)
        writeChan chan (pName ++ " is done eating")
        putMVar f1State f1
        writeChan chan (pName ++ " put " ++ f1Name ++ " back")
        putMVar f2State f2
        writeChan chan (pName ++ " put " ++ f2Name ++ " back")
        putMVar pState Thinking
        writeChan chan (pName ++ " is thinking")
