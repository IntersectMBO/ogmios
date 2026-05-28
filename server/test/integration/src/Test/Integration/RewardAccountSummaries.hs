{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.RewardAccountSummaries
  ( rewardAccountSummariesTests
  ) where

import Data.Aeson
    ( Value(..)
    , (.:)
    , eitherDecode
    , encode
    , object
    , (.=)
    )
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Foldable (toList)
import Data.Text (Text)
import System.FilePath ((</>))
import System.Process (readProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Network.WebSockets as WS

import Test.Integration.Env (TestEnv(..), queryOgmiosRetry)

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

      -- Query ogmios
      ogmiosResp <- queryOgmiosRetry (envOgmiosPort env) (\p -> queryOgmios p stakeAddr)
      ogmiosResult <- case ogmiosResp of
        Object o
          | Just result <- KM.lookup "result" o -> pure result
          | Just err    <- KM.lookup "error"  o ->
              assertFailure $ "Ogmios returned error: " <> show err
        _ -> assertFailure $ "Unexpected ogmios response: " <> show ogmiosResp

      -- Query cardano-cli
      cliResult <- queryCli (envWorkDir env) (envNodeSocket env) (envTestnetMagic env) stakeAddr

      ogmiosRewards <- case parseOgmiosRewards ogmiosResult of
        Left err -> assertFailure $ "Failed to parse ogmios rewards: " <> err
        Right v  -> pure v

      -- Parse rewards from cardano-cli: array of objects
      cliRewards <- case parseCliRewards cliResult of
        Left err -> assertFailure $ "Failed to parse cli rewards: " <> err
        Right v  -> pure v

      assertEqual "reward balance (lovelace)" ogmiosRewards cliRewards
  ]

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

queryOgmios :: Int -> String -> IO Value
queryOgmios port stakeAddr =
  WS.runClient "127.0.0.1" port "/" $ \conn -> do
    WS.sendTextData conn $ encode $ object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "method"  .= ("queryLedgerState/rewardAccountSummaries" :: Text)
      , "params"  .= object [ "keys" .= [ stakeAddr ] ]
      , "id"      .= Null
      ]
    resp <- WS.receiveData conn
    case eitherDecode resp of
      Left err  -> fail $ "Failed to decode ogmios response: " <> err
      Right val -> pure val

queryCli :: FilePath -> FilePath -> Int -> String -> IO Value
queryCli workDir socketPath magic stakeAddr = do
  let outFile = workDir </> "cli-stake-address-info.json"
  output <- readProcess "cardano-cli"
    [ "conway", "query", "stake-address-info"
    , "--address", stakeAddr
    , "--testnet-magic", show magic
    , "--socket-path", socketPath
    , "--out-file", outFile
    ] ""
  contents <- LBS.readFile outFile
  case eitherDecode contents of
    Left err  -> fail $ "Failed to decode cardano-cli output: " <> err <> "\nraw: " <> output
    Right val -> pure val

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

-- Ogmios returns an array of objects: [{ "credential": "...", "rewards": { "ada": { "lovelace": N } }, ... }]
parseOgmiosRewards :: Value -> Either String Integer
parseOgmiosRewards val = parseEither parser val
  where
    parser = withArray "summaries" $ \arr -> case toList arr of
      []    -> fail "No reward account summaries returned"
      (x:_) -> flip (withObject "entry") x $ \o -> do
        rewards <- o .: "rewards"
        ada <- rewards .: "ada"
        ada .: "lovelace"

-- cardano-cli returns: [ { "address": "...", "rewardAccountBalance": N, ... } ]
parseCliRewards :: Value -> Either String Integer
parseCliRewards val = parseEither parser val
  where
    parser = withArray "info" $ \arr -> case toList arr of
      []    -> fail "No stake address info returned"
      (x:_) -> flip (withObject "entry") x $ \o ->
        o .: "rewardAccountBalance"
