{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.Constitution
  ( constitutionTests
  ) where

import Data.Aeson (Value, (.:))
import Data.Aeson.Types (Parser, withObject)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Test.Integration.Env (TestEnv)
import Test.Integration.Query (parseIO, queryCli, queryOgmios)

constitutionTests :: IO TestEnv -> TestTree
constitutionTests getEnv = testGroup "Constitution"
  [ testCase "constitution matches cardano-cli" $ do
      env <- getEnv

      oHash <- parseIO "ogmios constitution" parseOgmiosHash
        =<< queryOgmios env "queryLedgerState/constitution" Nothing

      cHash <- parseIO "cli constitution" parseCliHash
        =<< queryCli env ["conway", "query", "constitution"]

      assertEqual "constitution anchor hash" oHash cHash
  ]

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

parseOgmiosHash :: Value -> Parser Text
parseOgmiosHash = withObject "constitution" $ \o -> do
  metadata <- o .: "metadata"
  metadata .: "hash"

parseCliHash :: Value -> Parser Text
parseCliHash = withObject "constitution" $ \o -> do
  anchor <- o .: "anchor"
  anchor .: "dataHash"
