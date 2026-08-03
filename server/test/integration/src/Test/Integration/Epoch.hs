{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.Epoch
  ( epochTests
  ) where

import Data.Aeson ((.:), parseJSON)
import Data.Aeson.Types (withObject)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Test.Integration.Env (TestEnv)
import Test.Integration.Query (parseIO, queryCli, queryOgmios)

epochTests :: IO TestEnv -> TestTree
epochTests getEnv = testGroup "Epoch"
  [ testCase "epoch matches cardano-cli tip epoch" $ do
      env <- getEnv

      ogmiosEpoch <- parseIO "ogmios epoch" parseJSON
        =<< queryOgmios env "queryLedgerState/epoch" Nothing

      cliEpoch <- parseIO "cli tip epoch" (withObject "tip" (.: "epoch"))
        =<< queryCli env ["conway", "query", "tip"]

      assertEqual "epoch" (ogmiosEpoch :: Integer) cliEpoch
  ]
