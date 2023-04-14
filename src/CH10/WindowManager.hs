module CH10.WindowManager where

import Data.Map
import Data.Set (Set)
import Data.Set qualified as Set
import Control.Concurrent.STM
    ( atomically, readTVar, writeTVar, STM, TVar, retry )

data Desktop = Desktop
  deriving (Eq, Ord)

data Window = Window
  deriving (Eq, Ord)

type Display = Map Desktop (TVar (Set Window))

moveWindowSTM :: Display -> Window -> Desktop -> Desktop -> STM ()
moveWindowSTM disp win a b = do
  wa <- readTVar ma
  wb <- readTVar mb
  writeTVar ma (Set.delete win wa)
  writeTVar mb (Set.insert win wb)
 where
  ma = disp ! a
  mb = disp ! b

swapWindows :: Display
            -> Window -> Desktop
            -> Window -> Desktop
            -> IO ()
swapWindows disp w a v b = atomically $ do
  moveWindowSTM disp w a b
  moveWindowSTM disp v b a

newtype TMVar a = TMVar (TVar (Maybe a))

takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a -> do
      writeTVar t Nothing
      pure a

putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar t) a = do
  m <- readTVar t
  case m of
    Nothing -> do
      writeTVar t (Just a)
      pure ()
    Just _ -> retry


