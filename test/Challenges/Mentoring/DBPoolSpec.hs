module Challenges.Mentoring.DBPoolSpec where

import Test.Hspec
import Challenges.Mentoring.DBPool
import Data.UUID.V4
import Data.UUID qualified as UUID
import Control.Concurrent.STM
import Control.Exception 
import Control.Concurrent 
import Control.Monad (void)


spec :: Spec
spec =
  describe "withConn" do
    it "Resources are acquired and freed synchronously" do
      uuid <- nextRandom
      connPool <- initConnPool $ pure uuid

      freeConns <- readTVarIO $ freeConnections connPool
      length (connections connPool) `shouldBe` length freeConns

      withConn connPool \conn -> do 
        cs <- readTVarIO $ connState conn
        cs `shouldBe` uuid

        freeConns' <- readTVarIO $ freeConnections connPool
        length freeConns' `shouldBe` length (connections connPool) - 1 

      freeConns' <- readTVarIO $ freeConnections connPool
      length freeConns' `shouldBe` length (connections connPool)
      

    it "Resources are acquired and freed asynchronously" do
      uuid <- nextRandom
      connPool <- initConnPool $ pure uuid

      freeConns <- readTVarIO $ freeConnections connPool
      length (connections connPool) `shouldBe` length freeConns

      threadLock <- newMVar ()
      connLock <- newMVar ()
      finishedLock <- newMVar ()
      
      -- Initialize all our locks in a locked state
      takeMVar connLock
      takeMVar threadLock
      takeMVar finishedLock

      -- Fork the "withConn" action so we can make subsequent assertions asynchronously
      void $ forkIO $ do
        withConn connPool \_ -> do 
          putMVar connLock () -- Connection has been acquired so release
          takeMVar threadLock -- Block the "withConn" thread so we can check that a free thread has been used
        putMVar finishedLock () -- Release the finished lock so that we can check that the connection was released

      -- Block on the connection being acquired
      takeMVar connLock 
      freeConns' <- readTVarIO $ freeConnections connPool
      length freeConns' `shouldBe` length (connections connPool) - 1 

      -- Release the thread lock so that the "withConn" job can finish
      putMVar threadLock () 

      -- Block until the "withConn" action has finished so we can check if the connection was released
      takeMVar finishedLock
      freeConns'' <- readTVarIO $ freeConnections connPool
      length freeConns'' `shouldBe` length (connections connPool) 

    it "Should release connection when a job throws" do
      uuid <- nextRandom
      connPool <- initConnPool $ pure ("" :: String)
      freeConns <- readTVarIO $ freeConnections connPool
      length (connections connPool) `shouldBe` length freeConns

      let ue = userError (UUID.toString uuid)

      shouldThrow 
        (withConn connPool (const $ throwIO ue))
        (\(e :: IOException) -> e == ue)

      freeConns' <- readTVarIO $ freeConnections connPool
      length (connections connPool) `shouldBe` length freeConns'
