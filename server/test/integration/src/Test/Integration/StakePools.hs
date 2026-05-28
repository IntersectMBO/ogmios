{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.StakePools
  ( stakePoolsTests
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

stakePoolsTests :: IO TestEnv -> TestTree
stakePoolsTests getEnv = testGroup "StakePools"
  [ testCase "stakePool IDs match cardano-cli" $ do
      env <- getEnv

      ogmiosResp <- queryOgmios (envOgmiosPort env)
      ogmiosResult <- case ogmiosResp of
        Object o
          | Just result <- KM.lookup "result" o -> pure result
          | Just err    <- KM.lookup "error"  o ->
              assertFailure $ "Ogmios returned error: " <> show err
        _ -> assertFailure $ "Unexpected ogmios response: " <> show ogmiosResp

      cliResult <- queryCli (envWorkDir env) (envNodeSocket env) (envTestnetMagic env)

      let ogmiosPoolIds = parseOgmiosPoolIds ogmiosResult
          cliPoolIds    = parseCliPoolIds cliResult

      ogmiosSet <- case ogmiosPoolIds of
        Left err -> assertFailure $ "Failed to parse ogmios pool IDs: " <> err
        Right s  -> pure s
      cliSet <- case cliPoolIds of
        Left err -> assertFailure $ "Failed to parse cli pool IDs: " <> err
        Right s  -> pure s

      let ogmiosOnly = Set.difference ogmiosSet cliSet
          cliOnly    = Set.difference cliSet ogmiosSet
      when (not (Set.null ogmiosOnly) || not (Set.null cliOnly)) $
        assertFailure $ unlines
          [ "Stake pool ID sets differ:"
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
      , "method"  .= ("queryLedgerState/stakePools" :: Text)
      , "id"      .= Null
      ]
    resp <- WS.receiveData conn
    case eitherDecode resp of
      Left err  -> fail $ "Failed to decode ogmios response: " <> err
      Right val -> pure val

queryCli :: FilePath -> FilePath -> Int -> IO Value
queryCli workDir socketPath magic = do
  let outFile = workDir </> "cli-stake-pools.json"
  callProcess "cardano-cli"
    [ "conway", "query", "stake-pools"
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

-- Ogmios returns an object keyed by pool ID (bech32)
parseOgmiosPoolIds :: Value -> Either String (Set Text)
parseOgmiosPoolIds = parseEither $ withObject "pools" $ \o ->
  pure $ Set.fromList $ map Key.toText (KM.keys o)

-- cardano-cli stake-pools returns an array of bech32 pool IDs
parseCliPoolIds :: Value -> Either String (Set Text)
parseCliPoolIds = parseEither $ withArray "pools" $ \arr ->
  fmap Set.fromList $ mapM parseString (toList arr)
  where
    parseString (String s) = pure s
    parseString v = fail $ "Expected string, got: " <> show v
