{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.ProtocolParameters
  ( protocolParametersTests
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value, (.:))
import Data.Aeson.Key (Key)
import Data.Aeson.Types (withObject)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Test.Integration.Env (TestEnv)
import Test.Integration.Query (parseIO, queryCli, queryOgmios)

protocolParametersTests :: IO TestEnv -> TestTree
protocolParametersTests getEnv = testGroup "ProtocolParameters"
  [ testCase "protocolParameters match cardano-cli" $ do
      env <- getEnv

      ogmiosResult <- queryOgmios env "queryLedgerState/protocolParameters" Nothing
      cliResult <- queryCli env ["conway", "query", "protocol-parameters"]

      oMinFeeCoeff <- ogmiosResult `getField` ["minFeeCoefficient"]
      cMinFeeCoeff <- cliResult `getField` ["txFeePerByte"]
      assertEqual "minFeeCoefficient" oMinFeeCoeff cMinFeeCoeff

      oMinFeeConst <- ogmiosResult `getField` ["minFeeConstant", "ada", "lovelace"]
      cMinFeeConst <- cliResult `getField` ["txFeeFixed"]
      assertEqual "minFeeConstant" oMinFeeConst cMinFeeConst

      oMaxBlockBody <- ogmiosResult `getField` ["maxBlockBodySize", "bytes"]
      cMaxBlockBody <- cliResult `getField` ["maxBlockBodySize"]
      assertEqual "maxBlockBodySize" oMaxBlockBody cMaxBlockBody

      oMaxTxSize <- ogmiosResult `getField` ["maxTransactionSize", "bytes"]
      cMaxTxSize <- cliResult `getField` ["maxTxSize"]
      assertEqual "maxTxSize" oMaxTxSize cMaxTxSize

      oStakeDeposit <- ogmiosResult `getField` ["stakeCredentialDeposit", "ada", "lovelace"]
      cStakeDeposit <- cliResult `getField` ["stakeAddressDeposit"]
      assertEqual "stakeCredentialDeposit" oStakeDeposit cStakeDeposit

      oPoolDeposit <- ogmiosResult `getField` ["stakePoolDeposit", "ada", "lovelace"]
      cPoolDeposit <- cliResult `getField` ["stakePoolDeposit"]
      assertEqual "stakePoolDeposit" oPoolDeposit cPoolDeposit
  ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Drill through nested objects along a path of keys, failing on the first
-- missing one.
getField :: Value -> [Key] -> IO Value
getField = foldM $ \v k ->
  parseIO ("field " <> show k) (withObject "obj" (.: k)) v
