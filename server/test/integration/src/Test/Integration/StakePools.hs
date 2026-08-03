{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.StakePools
  ( stakePoolsTests
  ) where

import Data.Aeson (Value(..))
import Data.Aeson.Types (Parser, withArray, withObject)
import Data.Foldable (toList)
import Data.Set (Set)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set

import Test.Integration.Env (TestEnv)
import Test.Integration.Query (assertSameSet, parseIO, queryCli, queryOgmios)

stakePoolsTests :: IO TestEnv -> TestTree
stakePoolsTests getEnv = testGroup "StakePools"
  [ testCase "stakePool IDs match cardano-cli" $ do
      env <- getEnv

      ogmiosSet <- parseIO "ogmios pool IDs" parseOgmiosPoolIds
        =<< queryOgmios env "queryLedgerState/stakePools" Nothing

      cliSet <- parseIO "cli pool IDs" parseCliPoolIds
        =<< queryCli env ["conway", "query", "stake-pools"]

      assertSameSet "Stake pool ID sets" ogmiosSet cliSet
  ]

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

-- Ogmios returns an object keyed by pool ID (bech32)
parseOgmiosPoolIds :: Value -> Parser (Set Text)
parseOgmiosPoolIds = withObject "pools" $ \o ->
  pure $ Set.fromList $ map Key.toText (KM.keys o)

-- cardano-cli stake-pools returns an array of bech32 pool IDs
parseCliPoolIds :: Value -> Parser (Set Text)
parseCliPoolIds = withArray "pools" $ \arr ->
  fmap Set.fromList $ mapM parseString (toList arr)
  where
    parseString (String s) = pure s
    parseString v = fail $ "Expected string, got: " <> show v
