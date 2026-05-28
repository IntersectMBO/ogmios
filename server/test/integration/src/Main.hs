module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Integration.Env (withTestEnv)
import Test.Integration.Utxo (utxoTests)

main :: IO ()
main = defaultMain $
  withTestEnv $ \getEnv ->
    testGroup "Ogmios Integration Tests"
      [ utxoTests getEnv
      ]
