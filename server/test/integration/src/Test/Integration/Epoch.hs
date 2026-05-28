{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.Epoch
  ( epochTests
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

import Test.Integration.Env (TestEnv(..))

epochTests :: IO TestEnv -> TestTree
epochTests getEnv = testGroup "Epoch"
  [ testCase "epoch matches cardano-cli tip epoch" $ do
      env <- getEnv

      ogmiosResp <- queryOgmios (envOgmiosPort env)
      ogmiosEpoch <- case ogmiosResp of
        Object o
          | Just (Number n) <- KM.lookup "result" o -> pure (round n :: Integer)
          | Just err        <- KM.lookup "error"  o ->
              assertFailure $ "Ogmios returned error: " <> show err
        _ -> assertFailure $ "Unexpected ogmios response: " <> show ogmiosResp

      cliResult <- queryCli (envWorkDir env) (envNodeSocket env) (envTestnetMagic env)
      cliEpoch <- case parseEither (withObject "tip" (.: "epoch")) cliResult of
        Left err -> assertFailure $ "Missing epoch in cli tip: " <> err
        Right v  -> pure (v :: Integer)

      assertEqual "epoch" ogmiosEpoch cliEpoch
  ]

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

queryOgmios :: Int -> IO Value
queryOgmios port =
  WS.runClient "127.0.0.1" port "/" $ \conn -> do
    WS.sendTextData conn $ encode $ object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "method"  .= ("queryLedgerState/epoch" :: Text)
      , "id"      .= Null
      ]
    resp <- WS.receiveData conn
    case eitherDecode resp of
      Left err  -> fail $ "Failed to decode ogmios response: " <> err
      Right val -> pure val

queryCli :: FilePath -> FilePath -> Int -> IO Value
queryCli workDir socketPath magic = do
  let outFile = workDir </> "cli-epoch-tip.json"
  callProcess "cardano-cli"
    [ "conway", "query", "tip"
    , "--testnet-magic", show magic
    , "--socket-path", socketPath
    , "--out-file", outFile
    ]
  contents <- LBS.readFile outFile
  case eitherDecode contents of
    Left err  -> fail $ "Failed to decode cardano-cli output: " <> err
    Right val -> pure val
