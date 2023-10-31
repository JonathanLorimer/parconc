module Main where

import Data.Maybe (fromMaybe)
import Spec qualified
import System.Environment (lookupEnv)
import Test.Hspec (parallel)
import Test.Hspec.Core.Formatters.V1 (specdoc)
import Test.Hspec.Core.Runner (Config (..))
import Test.Hspec.Runner as TR (defaultConfig, hspecWith)
import Text.Read (readMaybe)

main :: IO ()
main = do
  mText <- lookupEnv "TEST_CONCURRENCY"
  let
    maxResources :: Int
    maxResources = fromMaybe 4 (mText >>= readMaybe)
  let
    cfg =
        TR.defaultConfig
          { configConcurrentJobs = Just maxResources
          , configFormatter = Just specdoc
          }
  hspecWith cfg (parallel Spec.spec)
