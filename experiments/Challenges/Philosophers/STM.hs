module Challenges.Philosophers.STM where

import Control.Concurrent.STM
import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM.TSem

data Fork = Fork

data Philosopher
  = Thinking
  | Hungry
  | Eating
  | Starved
  deriving (Eq, Show)


newtype PhilosopherDied = PhilosopherDied String deriving (Show)

instance Exception PhilosopherDied

diningPhilosophers :: MVar () -> IO ()
diningPhilosophers done = do
  mutex <- newTMVarIO ()

  fork1 <- atomically $ newTSem 1
  fork2 <- atomically $ newTSem 1
  fork3 <- atomically $ newTSem 1
  fork4 <- atomically $ newTSem 1
  fork5 <- atomically $ newTSem 1

  plato <- newTVarIO Thinking
  konfuzius <- newTVarIO Thinking
  socrates <- newTVarIO Thinking
  voltaire <- newTVarIO Thinking
  descartes <- newTVarIO Thinking

  forkIO $ philosopher mutex done (plato, "plato") (fork1, fork2) (descartes, konfuzius)
  forkIO $ philosopher mutex done (konfuzius, "konfuzius") (fork2, fork3) (plato, socrates)
  forkIO $ philosopher mutex done (socrates, "socrates") (fork3, fork4) (konfuzius, voltaire)
  forkIO $ philosopher mutex done (voltaire, "voltaire") (fork4, fork5) (socrates, descartes)
  forkIO $ philosopher mutex done (descartes, "descartes") (fork5, fork1) (voltaire, plato)

  -- forkIO $ forever do
  --   threadDelay (4 * 10 ^ 5)
  --   states <- mapM readTVarIO
  --     [ plato
  --     , konfuzius
  --     , socrates
  --     , voltaire
  --     , descartes
  --     ]
  --   print $ zip states
  --     [ "plato"
  --     , "konfuzius"
  --     , "socrates"
  --     , "voltaire"
  --     , "descartes"
  --     ]
  --
  threadDelay (30 * 10 ^ 6)

  pure ()

philosopher
  :: TMVar ()
  -> MVar ()
  -> (TVar Philosopher, String)
  -> (TSem, TSem)
  -> (TVar Philosopher, TVar Philosopher)
  -> IO ()
philosopher mutex done self forks neighbours = forever do
  think mutex self
  acquiringForks mutex done self forks
  eat mutex self forks
  atomically $ releasingForks self forks
  atomically $ uncurry waitOnNeighbors neighbours

mutexPrint :: TMVar () -> String -> IO ()
mutexPrint mutex msg = do
  atomically $ takeTMVar mutex
  print msg
  atomically $ putTMVar mutex ()

think :: TMVar () -> (TVar Philosopher, String) -> IO ()
think mutex (state, name) = do
  atomically $ writeTVar state Thinking
  mutexPrint mutex (name ++ " is thinking")
  threadDelay (1 * 10 ^ 6)

acquiringForks :: TMVar () -> MVar () -> (TVar Philosopher, String) -> (TSem, TSem) -> IO ()
acquiringForks mutex done (state, name) (forkL, forkR) = do
  tid <- myThreadId
  bracket (forkIO do
              threadDelay (10 * (10 ^ 6))
              atomically $ writeTVar state Starved
              putMVar done ()
              throwTo tid (PhilosopherDied name))
          (`throwTo` ThreadKilled)
          (\_ -> do
              atomically $ writeTVar state Hungry
              mutexPrint mutex (name ++ " is hungry")
              atomically $ do
                waitTSem forkL
                waitTSem forkR)

eat :: TMVar () -> (TVar Philosopher, String) -> (TSem, TSem) -> IO ()
eat mutex (state, name) (forkL, forkR) = do
  atomically $ writeTVar state Eating
  mutexPrint mutex (name ++ " is eating")
  threadDelay (1 * 10 ^ 6)

releasingForks :: (TVar Philosopher, String) -> (TSem, TSem) -> STM ()
releasingForks (state, _) (forkL, forkR) = do
  signalTSem forkL
  signalTSem forkR

waitOnNeighbors :: TVar Philosopher -> TVar Philosopher -> STM ()
waitOnNeighbors left right = do
  l <- readTVar left
  r <- readTVar right
  check (l /= Hungry)
  check (r /= Hungry)



