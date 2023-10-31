module Challenges.Mentoring.DBPoolSpec where

import Test.Hspec
import Challenges.Mentoring.DBPool
import Data.UUID.V4
import Control.Concurrent.STM


spec :: Spec
spec =
  describe "withConn" do
    it "should acquire connection, run job, and release connection" do
      uuid <- nextRandom
      connPool <- initConnPool $ pure uuid
      freeConns <- readTVarIO $ freeConnections connPool
      length (connections connPool) `shouldBe` length freeConns
      withConn connPool \conn -> do
        cs <- readTVarIO $ connState conn
        cs `shouldBe` uuid
      freeConns' <- readTVarIO $ freeConnections connPool
      length (connections connPool) `shouldBe` length freeConns'
        
      pure ()
