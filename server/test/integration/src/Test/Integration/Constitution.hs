{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.Constitution
  ( constitutionTests
  ) where

import Data.Aeson
    ( Value(..)
    , (.:)
    , eitherDecode
    , encode
    , object
    , (.=)
    )
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

constitutionTests :: IO TestEnv -> TestTree
constitutionTests getEnv = testGroup "Constitution"
  [ testCase "constitution matches cardano-cli" $ do
      env <- getEnv

      ogmiosResp <- queryOgmiosRetry (envOgmiosPort env) queryOgmios
      ogmiosResult <- case ogmiosResp of
        Object o
          | Just result <- KM.lookup "result" o -> pure result
          | Just err    <- KM.lookup "error"  o ->
              assertFailure $ "Ogmios returned error: " <> show err
        _ -> assertFailure $ "Unexpected ogmios response: " <> show ogmiosResp

      cliResult <- queryCli (envWorkDir env) (envNodeSocket env) (envTestnetMagic env)

      oHash <- parseOgmiosHash ogmiosResult
      cHash <- parseCliHash cliResult
      assertEqual "constitution anchor hash" oHash cHash
  ]

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

queryOgmios :: Int -> IO Value
queryOgmios port =
  WS.runClient "127.0.0.1" port "/" $ \conn -> do
    WS.sendTextData conn $ encode $ object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "method"  .= ("queryLedgerState/constitution" :: Text)
      , "id"      .= Null
      ]
    resp <- WS.receiveData conn
    case eitherDecode resp of
      Left err  -> fail $ "Failed to decode ogmios response: " <> err
      Right val -> pure val

queryCli :: FilePath -> FilePath -> Int -> IO Value
queryCli workDir socketPath magic = do
  let outFile = workDir </> "cli-constitution.json"
  callProcess "cardano-cli"
    [ "conway", "query", "constitution"
    , "--testnet-magic", show magic
    , "--socket-path", socketPath
    , "--out-file", outFile
    ]
  contents <- LBS.readFile outFile
  case eitherDecode contents of
    Left err  -> fail $ "Failed to decode cardano-cli output: " <> err
    Right val -> pure val

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

parseOgmiosHash :: Value -> IO Text
parseOgmiosHash val = case parseEither parser val of
    Left err -> assertFailure $ "Failed to parse ogmios constitution: " <> err
    Right v  -> pure v
  where
    parser = withObject "constitution" $ \o -> do
      metadata <- o .: "metadata"
      metadata .: "hash"

parseCliHash :: Value -> IO Text
parseCliHash val = case parseEither parser val of
    Left err -> assertFailure $ "Failed to parse cli constitution: " <> err
    Right v  -> pure v
  where
    parser = withObject "constitution" $ \o -> do
      anchor <- o .: "anchor"
      anchor .: "dataHash"
