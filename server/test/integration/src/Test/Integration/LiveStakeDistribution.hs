{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.LiveStakeDistribution
  ( liveStakeDistributionTests
  ) where

import Control.Monad (when)
import Data.Aeson
    ( Value(..)
    , eitherDecode
    , encode
    , object
    , (.=)
    )
import Data.Aeson.Types (parseEither, withObject)
import Data.Set (Set)
import Data.Text (Text)
import System.Process (readProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import qualified Network.WebSockets as WS

import Test.Integration.Env (TestEnv(..), queryOgmiosRetry)

liveStakeDistributionTests :: IO TestEnv -> TestTree
liveStakeDistributionTests getEnv = testGroup "LiveStakeDistribution"
  [ testCase "pool IDs in stake-distribution match" $ do
      env <- getEnv

      ogmiosResp <- queryOgmiosRetry (envOgmiosPort env) queryOgmios
      ogmiosResult <- case ogmiosResp of
        Object o
          | Just result <- KM.lookup "result" o -> pure result
          | Just err    <- KM.lookup "error"  o ->
              assertFailure $ "Ogmios returned error: " <> show err
        _ -> assertFailure $ "Unexpected ogmios response: " <> show ogmiosResp

      cliOutput <- queryCli (envNodeSocket env) (envTestnetMagic env)

      ogmiosPoolIds <- case parseOgmiosPoolIds ogmiosResult of
        Left err -> assertFailure $ "Failed to parse ogmios pools: " <> err
        Right s  -> pure s
      cliPoolIds <- case parseCliPoolIds cliOutput of
        Left err -> assertFailure $ "Failed to parse cli pools: " <> err
        Right s  -> pure s

      let ogmiosOnly = Set.difference ogmiosPoolIds cliPoolIds
          cliOnly    = Set.difference cliPoolIds ogmiosPoolIds
      when (not (Set.null ogmiosOnly) || not (Set.null cliOnly)) $
        assertFailure $ unlines
          [ "Stake distribution pool ID sets differ:"
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
      , "method"  .= ("queryLedgerState/liveStakeDistribution" :: Text)
      , "id"      .= Null
      ]
    resp <- WS.receiveData conn
    case eitherDecode resp of
      Left err  -> fail $ "Failed to decode ogmios response: " <> err
      Right val -> pure val

-- cardano-cli stake-distribution outputs a text table, use --output-json
queryCli :: FilePath -> Int -> IO Value
queryCli socketPath magic = do
  output <- readProcess "cardano-cli"
    [ "conway", "query", "stake-distribution"
    , "--testnet-magic", show magic
    , "--socket-path", socketPath
    , "--output-json"
    ] ""
  case eitherDecode (LBS.fromStrict $ encodeUtf8 output) of
    Left err  -> fail $ "Failed to decode cardano-cli output: " <> err
    Right val -> pure val
  where
    encodeUtf8 = LBS.toStrict . LBS.pack . map (fromIntegral . fromEnum)

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

-- Ogmios returns an object keyed by pool ID
parseOgmiosPoolIds :: Value -> Either String (Set Text)
parseOgmiosPoolIds = parseEither $ withObject "distribution" $ \o ->
  pure $ Set.fromList $ map Key.toText (KM.keys o)

-- cardano-cli --output-json returns an object keyed by pool ID
parseCliPoolIds :: Value -> Either String (Set Text)
parseCliPoolIds = parseEither $ withObject "distribution" $ \o ->
  pure $ Set.fromList $ map Key.toText (KM.keys o)
