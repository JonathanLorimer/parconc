module Challenges.Philosophers.STM where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Control.Monad

data Fork = Fork

delayMilliSecs :: Int -> Int
delayMilliSecs = (*) ((10 :: Int) ^ (6 :: Int))

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

  void $ forkIO $ philosopher mutex done (plato, "plato") (fork1, fork2) (descartes, konfuzius)
  void $ forkIO $ philosopher mutex done (konfuzius, "konfuzius") (fork2, fork3) (plato, socrates)
  void $ forkIO $ philosopher mutex done (socrates, "socrates") (fork3, fork4) (konfuzius, voltaire)
  void $ forkIO $ philosopher mutex done (voltaire, "voltaire") (fork4, fork5) (socrates, descartes)
  void $ forkIO $ philosopher mutex done (descartes, "descartes") (fork5, fork1) (voltaire, plato)

  threadDelay (delayMilliSecs 30)

  pure ()

-- nicer logging if you want it
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

philosopher ::
  TMVar () ->
  MVar () ->
  (TVar Philosopher, String) ->
  (TSem, TSem) ->
  (TVar Philosopher, TVar Philosopher) ->
  IO ()
philosopher mutex done self forks neighbours = forever do
  think mutex self
  acquiringForks mutex done self forks
  eat mutex self
  atomically $ releasingForks forks
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
  threadDelay (delayMilliSecs 1)

acquiringForks :: TMVar () -> MVar () -> (TVar Philosopher, String) -> (TSem, TSem) -> IO ()
acquiringForks mutex done (state, name) (forkL, forkR) = do
  tid <- myThreadId
  bracket
    ( forkIO do
        threadDelay (delayMilliSecs 10)
        atomically $ writeTVar state Starved
        putMVar done ()
        throwTo tid (PhilosopherDied name)
    )
    (`throwTo` ThreadKilled)
    ( \_ -> do
        atomically $ writeTVar state Hungry
        mutexPrint mutex (name ++ " is hungry")
        atomically $ do
          waitTSem forkL
          waitTSem forkR
    )

eat :: TMVar () -> (TVar Philosopher, String) -> IO ()
eat mutex (state, name) = do
  atomically $ writeTVar state Eating
  mutexPrint mutex (name ++ " is eating")
  threadDelay (delayMilliSecs 1)

releasingForks :: (TSem, TSem) -> STM ()
releasingForks (forkL, forkR) = do
  signalTSem forkL
  signalTSem forkR

waitOnNeighbors :: TVar Philosopher -> TVar Philosopher -> STM ()
waitOnNeighbors left right = do
  l <- readTVar left
  r <- readTVar right
  check (l /= Hungry)
  check (r /= Hungry)
