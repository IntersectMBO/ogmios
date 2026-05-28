{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.NetworkTip
  ( networkTipTests
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
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Network.WebSockets as WS

import Test.Integration.Env (TestEnv(..))

networkTipTests :: IO TestEnv -> TestTree
networkTipTests getEnv = testGroup "NetworkTip"
  [ testCase "network/tip slot is bracketed by cardano-cli tip" $ do
      env <- getEnv

      cliBefore <- queryCli (envWorkDir env) (envNodeSocket env) (envTestnetMagic env) "cli-network-tip-before.json"
      cSlotBefore <- parseField cliBefore "slot"

      ogmiosResp <- queryOgmios (envOgmiosPort env)
      ogmiosResult <- case ogmiosResp of
        Object o
          | Just result <- KM.lookup "result" o -> pure result
          | Just err    <- KM.lookup "error"  o ->
              assertFailure $ "Ogmios returned error: " <> show err
        _ -> assertFailure $ "Unexpected ogmios response: " <> show ogmiosResp
      oSlot <- parseField ogmiosResult "slot"

      cliAfter <- queryCli (envWorkDir env) (envNodeSocket env) (envTestnetMagic env) "cli-network-tip-after.json"
      cSlotAfter <- parseField cliAfter "slot"

      assertBool
        ("Expected cli_before <= ogmios <= cli_after, got: "
         <> show cSlotBefore <> " <= " <> show oSlot <> " <= " <> show cSlotAfter)
        (cSlotBefore <= oSlot && oSlot <= cSlotAfter)
  ]

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

queryOgmios :: Int -> IO Value
queryOgmios port =
  WS.runClient "127.0.0.1" port "/" $ \conn -> do
    WS.sendTextData conn $ encode $ object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "method"  .= ("queryNetwork/tip" :: Text)
      , "id"      .= Null
      ]
    resp <- WS.receiveData conn
    case eitherDecode resp of
      Left err  -> fail $ "Failed to decode ogmios response: " <> err
      Right val -> pure val

queryCli :: FilePath -> FilePath -> Int -> FilePath -> IO Value
queryCli workDir socketPath magic outName = do
  let outFile = workDir </> outName
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

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

parseField :: Value -> Key -> IO Integer
parseField val key = case parseEither (withObject "obj" (.: key)) val of
  Left err -> assertFailure $ "Missing field " <> show key <> ": " <> err
  Right v  -> pure v
