{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.Utxo
  ( utxoTests
  ) where

import Control.Monad (forM, when)
import Data.Aeson
    ( Value(..)
    , (.:)
    , eitherDecode
    , encode
    , object
    , (.=)
    )
import Data.Aeson.Types (Parser, parseEither, withArray, withObject)
import Data.Foldable (toList)
import Data.Set (Set)
import Data.Text (Text)
import System.FilePath ((</>))
import System.Process (callProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.Read (readMaybe)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Test.Integration.Env (TestEnv(..))

-- ---------------------------------------------------------------------------
-- Normalized UTxO representation
-- ---------------------------------------------------------------------------

data NormalizedUtxo = NormalizedUtxo
  { nuTxId     :: !Text
  , nuTxIndex  :: !Int
  , nuAddress  :: !Text
  , nuLovelace :: !Integer
  } deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

utxoTests :: IO TestEnv -> TestTree
utxoTests getEnv = testGroup "UTxO"
  [ testCase "WholeUtxo matches cardano-cli" $ do
      env <- getEnv

      -- Query ogmios via WebSocket
      ogmiosResp <- queryOgmiosWholeUtxo (envOgmiosPort env)
      ogmiosResult <- case ogmiosResp of
        Object o
          | Just result <- KM.lookup "result" o -> pure result
          | Just err    <- KM.lookup "error"  o ->
              assertFailure $ "Ogmios returned error: " <> show err
        _ -> assertFailure $ "Unexpected ogmios response shape: " <> show ogmiosResp

      ogmiosUtxos <- case parseOgmiosUtxo ogmiosResult of
        Left err   -> assertFailure $ "Failed to parse ogmios UTxO: " <> err
        Right utxos -> pure utxos

      -- Query cardano-cli
      cliOutput <- queryCardanoCliWholeUtxo
        (envWorkDir env) (envNodeSocket env) (envTestnetMagic env)
      cliUtxos <- case parseCardanoCliUtxo cliOutput of
        Left err   -> assertFailure $ "Failed to parse cardano-cli UTxO: " <> err
        Right utxos -> pure utxos

      -- Compare
      let ogmiosOnly = Set.difference ogmiosUtxos cliUtxos
          cliOnly    = Set.difference cliUtxos ogmiosUtxos
      when (not (Set.null ogmiosOnly) || not (Set.null cliOnly)) $
        assertFailure $ unlines
          [ "UTxO sets differ:"
          , "  In Ogmios only (" <> show (Set.size ogmiosOnly) <> "):"
          , concatMap (\e -> "    " <> show e <> "\n") (Set.toList ogmiosOnly)
          , "  In cardano-cli only (" <> show (Set.size cliOnly) <> "):"
          , concatMap (\e -> "    " <> show e <> "\n") (Set.toList cliOnly)
          ]
  ]

-- ---------------------------------------------------------------------------
-- Ogmios query
-- ---------------------------------------------------------------------------

queryOgmiosWholeUtxo :: Int -> IO Value
queryOgmiosWholeUtxo port =
  WS.runClient "127.0.0.1" port "/" $ \conn -> do
    let req = encode $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "method"  .= ("queryLedgerState/utxo" :: Text)
          , "params"  .= object []
          , "id"      .= Null
          ]
    WS.sendTextData conn req
    resp <- WS.receiveData conn
    case eitherDecode resp of
      Left err  -> fail $ "Failed to decode ogmios response: " <> err
      Right val -> pure val

-- ---------------------------------------------------------------------------
-- cardano-cli query
-- ---------------------------------------------------------------------------

queryCardanoCliWholeUtxo :: FilePath -> FilePath -> Int -> IO Value
queryCardanoCliWholeUtxo workDir socketPath magic = do
  let outFile = workDir </> "cli-utxo.json"
  callProcess "cardano-cli"
    [ "conway", "query", "utxo"
    , "--whole-utxo"
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

parseOgmiosUtxo :: Value -> Either String (Set NormalizedUtxo)
parseOgmiosUtxo = parseEither $ withArray "utxo" $ \arr -> do
  entries <- mapM parseEntry (toList arr)
  pure (Set.fromList entries)
  where
    parseEntry = withObject "utxo entry" $ \o -> do
      tx   <- o .: "transaction"
      txId <- tx .: "id"
      idx  <- o .: "index"
      addr <- o .: "address"
      val  <- o .: "value"
      ada  <- val .: "ada"
      lv   <- ada .: "lovelace"
      pure NormalizedUtxo
        { nuTxId     = txId
        , nuTxIndex  = idx
        , nuAddress  = addr
        , nuLovelace = lv
        }

parseCardanoCliUtxo :: Value -> Either String (Set NormalizedUtxo)
parseCardanoCliUtxo = parseEither $ withObject "utxo set" $ \o -> do
  entries <- forM (KM.toList o) $ \(key, val) -> do
    let keyText      = Key.toText key
        (txId, rest) = T.breakOn "#" keyText
        idxText      = T.drop 1 rest
    idx <- case readMaybe (T.unpack idxText) of
      Just n  -> pure n
      Nothing -> fail $ "Invalid UTxO key (expected txid#index): " <> T.unpack keyText
    parseOutput txId idx val
  pure (Set.fromList entries)

parseOutput :: Text -> Int -> Value -> Parser NormalizedUtxo
parseOutput txId idx = withObject "utxo output" $ \o -> do
  addr <- o .: "address"
  val  <- o .: "value"
  lv   <- val .: "lovelace"
  pure NormalizedUtxo
    { nuTxId     = txId
    , nuTxIndex  = idx
    , nuAddress  = addr
    , nuLovelace = lv
    }
