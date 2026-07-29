{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.ProtocolParameters
  ( protocolParametersTests
  ) where

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

      oMinFeeCoeff <- field1 ogmiosResult "minFeeCoefficient"
      cMinFeeCoeff <- field1 cliResult "txFeePerByte"
      assertEqual "minFeeCoefficient" oMinFeeCoeff cMinFeeCoeff

      oMinFeeConst <- field3 ogmiosResult "minFeeConstant" "ada" "lovelace"
      cMinFeeConst <- field1 cliResult "txFeeFixed"
      assertEqual "minFeeConstant" oMinFeeConst cMinFeeConst

      oMaxBlockBody <- field2 ogmiosResult "maxBlockBodySize" "bytes"
      cMaxBlockBody <- field1 cliResult "maxBlockBodySize"
      assertEqual "maxBlockBodySize" oMaxBlockBody cMaxBlockBody

      oMaxTxSize <- field2 ogmiosResult "maxTransactionSize" "bytes"
      cMaxTxSize <- field1 cliResult "maxTxSize"
      assertEqual "maxTxSize" oMaxTxSize cMaxTxSize

      oStakeDeposit <- field3 ogmiosResult "stakeCredentialDeposit" "ada" "lovelace"
      cStakeDeposit <- field1 cliResult "stakeAddressDeposit"
      assertEqual "stakeCredentialDeposit" oStakeDeposit cStakeDeposit

      oPoolDeposit <- field3 ogmiosResult "stakePoolDeposit" "ada" "lovelace"
      cPoolDeposit <- field1 cliResult "stakePoolDeposit"
      assertEqual "stakePoolDeposit" oPoolDeposit cPoolDeposit
  ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

field1 :: Value -> Key -> IO Value
field1 val k = parseIO ("field " <> show k) (withObject "obj" (.: k)) val

field2 :: Value -> Key -> Key -> IO Value
field2 val k1 k2 = field1 val k1 >>= \v -> field1 v k2

field3 :: Value -> Key -> Key -> Key -> IO Value
field3 val k1 k2 k3 = field2 val k1 k2 >>= \v -> field1 v k3
