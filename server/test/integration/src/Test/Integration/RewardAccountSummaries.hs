{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.RewardAccountSummaries
  ( rewardAccountSummariesTests
  ) where

import Data.Aeson (Value, (.:), object, (.=))
import Data.Aeson.Types (Parser, withArray, withObject)
import Data.Foldable (toList)
import System.FilePath ((</>))
import System.Process (readProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import Test.Integration.Env (TestEnv(..))
import Test.Integration.Query (parseIO, queryCli, queryOgmios)

rewardAccountSummariesTests :: IO TestEnv -> TestTree
rewardAccountSummariesTests getEnv = testGroup "RewardAccountSummaries"
  [ testCase "rewardAccountSummaries match cardano-cli stake-address-info" $ do
      env <- getEnv

      -- Derive stake address from the first delegator's staking key
      let stakingVkey = envTestnetDir env </> "stake-delegators"
                        </> "delegator1" </> "staking.vkey"
      output <- readProcess "cardano-cli"
        [ "conway", "stake-address", "build"
        , "--stake-verification-key-file", stakingVkey
        , "--testnet-magic", show (envTestnetMagic env)
        ] ""
      stakeAddr <- case lines output of
        (addr:_) -> pure addr
        []       -> assertFailure "cardano-cli stake-address build returned empty output"

      ogmiosRewards <- parseIO "ogmios rewards" parseOgmiosRewards
        =<< queryOgmios env "queryLedgerState/rewardAccountSummaries"
              (Just (object [ "keys" .= [ stakeAddr ] ]))

      cliRewards <- parseIO "cli rewards" parseCliRewards
        =<< queryCli env ["conway", "query", "stake-address-info", "--address", stakeAddr]

      assertEqual "reward balance (lovelace)" ogmiosRewards cliRewards
  ]

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

-- Ogmios returns an array of objects: [{ "credential": "...", "rewards": { "ada": { "lovelace": N } }, ... }]
parseOgmiosRewards :: Value -> Parser Integer
parseOgmiosRewards = withArray "summaries" $ \arr -> case toList arr of
  []    -> fail "No reward account summaries returned"
  (x:_) -> flip (withObject "entry") x $ \o -> do
    rewards <- o .: "rewards"
    ada <- rewards .: "ada"
    ada .: "lovelace"

-- cardano-cli returns: [ { "address": "...", "rewardAccountBalance": N, ... } ]
parseCliRewards :: Value -> Parser Integer
parseCliRewards = withArray "info" $ \arr -> case toList arr of
  []    -> fail "No stake address info returned"
  (x:_) -> flip (withObject "entry") x $ \o ->
    o .: "rewardAccountBalance"
