{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.DelegateRepresentatives
  ( delegateRepresentativesTests
  ) where

import Control.Monad (when)
import Data.Aeson
    ( Value(..)
    , eitherDecode
    , encode
    , object
    , (.=)
    )
import Data.Aeson.Types (parseEither, withArray, withObject)
import Data.Foldable (toList)
import Data.Set (Set)
import Data.Text (Text)
import System.FilePath ((</>))
import System.Process (callProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import qualified Network.WebSockets as WS

import Test.Integration.Env (TestEnv(..))

delegateRepresentativesTests :: IO TestEnv -> TestTree
delegateRepresentativesTests getEnv = testGroup "DelegateRepresentatives"
  [ testCase "DRep IDs match cardano-cli" $ do
      env <- getEnv

      ogmiosResp <- queryOgmios (envOgmiosPort env)
      ogmiosResult <- case ogmiosResp of
        Object o
          | Just result <- KM.lookup "result" o -> pure result
          | Just err    <- KM.lookup "error"  o ->
              assertFailure $ "Ogmios returned error: " <> show err
        _ -> assertFailure $ "Unexpected ogmios response: " <> show ogmiosResp

      cliResult <- queryCli (envWorkDir env) (envNodeSocket env) (envTestnetMagic env)

      ogmiosDreps <- case parseOgmiosDrepIds ogmiosResult of
        Left err -> assertFailure $ "Failed to parse ogmios DRep IDs: " <> err
        Right s  -> pure s
      cliDreps <- case parseCliDrepIds cliResult of
        Left err -> assertFailure $ "Failed to parse cli DRep IDs: " <> err
        Right s  -> pure s

      let ogmiosOnly = Set.difference ogmiosDreps cliDreps
          cliOnly    = Set.difference cliDreps ogmiosDreps
      when (not (Set.null ogmiosOnly) || not (Set.null cliOnly)) $
        assertFailure $ unlines
          [ "DRep ID sets differ:"
          , "  In Ogmios only: " <> show (Set.toList ogmiosOnly)
          , "  In cardano-cli only: " <> show (Set.toList cliOnly)
          ]
  ]

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

queryOgmios :: Int -> IO Value
queryOgmios port =
  WS.runClient "127.0.0.1" port "/" $ \conn -> do
    WS.sendTextData conn $ encode $ object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "method"  .= ("queryLedgerState/delegateRepresentatives" :: Text)
      , "id"      .= Null
      ]
    resp <- WS.receiveData conn
    case eitherDecode resp of
      Left err  -> fail $ "Failed to decode ogmios response: " <> err
      Right val -> pure val

queryCli :: FilePath -> FilePath -> Int -> IO Value
queryCli workDir socketPath magic = do
  let outFile = workDir </> "cli-drep-state.json"
  callProcess "cardano-cli"
    [ "conway", "query", "drep-state"
    , "--all-dreps"
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

-- Ogmios returns an array of objects with "id" and "type" fields.
-- Special entries "abstain" and "noConfidence" have no "id".
parseOgmiosDrepIds :: Value -> Either String (Set Text)
parseOgmiosDrepIds = parseEither $ withArray "dreps" $ \arr -> do
  ids <- mapM extractId (toList arr)
  pure $ Set.fromList [i | Just i <- ids]
  where
    extractId = withObject "drep" $ \o ->
      case KM.lookup "id" o of
        Just (String drepId) -> pure (Just drepId)
        _ -> pure Nothing

-- cardano-cli drep-state --all-dreps returns an array of [drepId, drepState] pairs
parseCliDrepIds :: Value -> Either String (Set Text)
parseCliDrepIds = parseEither $ withArray "dreps" $ \arr -> do
  ids <- mapM extractId (toList arr)
  pure (Set.fromList ids)
  where
    extractId = withArray "pair" $ \pair -> case toList pair of
      (drepId:_) -> case drepId of
        Object o -> case KM.lookup "keyHash" o of
          Just (String h) -> pure h
          _ -> case KM.lookup "scriptHash" o of
            Just (String h) -> pure h
            _ -> fail $ "No keyHash or scriptHash in DRep ID: " <> show o
        _ -> fail $ "Expected object for DRep ID, got: " <> show drepId
      _ -> fail "Empty DRep pair"
