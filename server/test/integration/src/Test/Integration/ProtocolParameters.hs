{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.ProtocolParameters
  ( protocolParametersTests
  ) where

import Data.Aeson
    ( Value(..)
    , (.:)
    , eitherDecode
    , encode
    , object
    , (.=)
    )
import Data.Aeson.Key (Key)
import Data.Aeson.Types (parseEither, withObject)
import Data.Text (Text)
import System.FilePath ((</>))
import System.Process (callProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Network.WebSockets as WS

import Test.Integration.Env (TestEnv(..), queryOgmiosRetry)

protocolParametersTests :: IO TestEnv -> TestTree
protocolParametersTests getEnv = testGroup "ProtocolParameters"
  [ testCase "protocolParameters match cardano-cli" $ do
      env <- getEnv

      ogmiosResp <- queryOgmiosRetry (envOgmiosPort env) queryOgmios
      ogmiosResult <- case ogmiosResp of
        Object o
          | Just result <- KM.lookup "result" o -> pure result
          | Just err    <- KM.lookup "error"  o ->
              assertFailure $ "Ogmios returned error: " <> show err
        _ -> assertFailure $ "Unexpected ogmios response: " <> show ogmiosResp

      cliResult <- queryCli (envWorkDir env) (envNodeSocket env) (envTestnetMagic env)

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
-- Queries
-- ---------------------------------------------------------------------------

queryOgmios :: Int -> IO Value
queryOgmios port =
  WS.runClient "127.0.0.1" port "/" $ \conn -> do
    WS.sendTextData conn $ encode $ object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "method"  .= ("queryLedgerState/protocolParameters" :: Text)
      , "id"      .= Null
      ]
    resp <- WS.receiveData conn
    case eitherDecode resp of
      Left err  -> fail $ "Failed to decode ogmios response: " <> err
      Right val -> pure val

queryCli :: FilePath -> FilePath -> Int -> IO Value
queryCli workDir socketPath magic = do
  let outFile = workDir </> "cli-protocol-parameters.json"
  callProcess "cardano-cli"
    [ "conway", "query", "protocol-parameters"
    , "--testnet-magic", show magic
    , "--socket-path", socketPath
    , "--out-file", outFile
    ]
  contents <- LBS.readFile outFile
  case eitherDecode contents of
    Left err  -> fail $ "Failed to decode cardano-cli output: " <> err
    Right val -> pure val

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

field1 :: Value -> Key -> IO Value
field1 val k = case parseEither (withObject "obj" (.: k)) val of
  Left err -> assertFailure $ "Missing field " <> show k <> ": " <> err
  Right v  -> pure v

field2 :: Value -> Key -> Key -> IO Value
field2 val k1 k2 = field1 val k1 >>= \v -> field1 v k2

field3 :: Value -> Key -> Key -> Key -> IO Value
field3 val k1 k2 k3 = field2 val k1 k2 >>= \v -> field1 v k3
