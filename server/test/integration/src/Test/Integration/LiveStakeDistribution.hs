{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.LiveStakeDistribution
  ( liveStakeDistributionTests
  ) where

import Data.Aeson (Value)
import Data.Aeson.Types (Parser, withObject)
import Data.Set (Set)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set

import Test.Integration.Env (TestEnv)
import Test.Integration.Query (assertSameSet, parseIO, queryCliStdout, queryOgmios)

liveStakeDistributionTests :: IO TestEnv -> TestTree
liveStakeDistributionTests getEnv = testGroup "LiveStakeDistribution"
  [ testCase "pool IDs in stake-distribution match" $ do
      env <- getEnv

      ogmiosPoolIds <- parseIO "ogmios pools" parsePoolIds
        =<< queryOgmios env "queryLedgerState/liveStakeDistribution" Nothing

      -- cardano-cli stake-distribution has no --out-file and prints a text
      -- table by default, so take JSON from stdout instead
      cliPoolIds <- parseIO "cli pools" parsePoolIds
        =<< queryCliStdout env ["conway", "query", "stake-distribution", "--output-json"]

      assertSameSet "Stake distribution pool ID sets" ogmiosPoolIds cliPoolIds
  ]

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

-- Both sides return an object keyed by pool ID
parsePoolIds :: Value -> Parser (Set Text)
parsePoolIds = withObject "distribution" $ \o ->
  pure $ Set.fromList $ map Key.toText (KM.keys o)
