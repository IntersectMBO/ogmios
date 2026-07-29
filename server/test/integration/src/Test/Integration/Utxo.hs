{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.Utxo
  ( utxoTests
  ) where

import Control.Monad (forM)
import Data.Aeson (Value, (.:), object)
import Data.Aeson.Types (Parser, withArray, withObject)
import Data.Foldable (toList)
import Data.Set (Set)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Text.Read (readMaybe)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
import qualified Data.Text as T

import Test.Integration.Env (TestEnv)
import Test.Integration.Query (assertSameSet, parseIO, queryCli, queryOgmios)

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

      ogmiosUtxos <- parseIO "ogmios UTxO" parseOgmiosUtxo
        =<< queryOgmios env "queryLedgerState/utxo" (Just (object []))

      cliUtxos <- parseIO "cardano-cli UTxO" parseCardanoCliUtxo
        =<< queryCli env ["conway", "query", "utxo", "--whole-utxo"]

      assertSameSet "UTxO sets" ogmiosUtxos cliUtxos
  ]

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

parseOgmiosUtxo :: Value -> Parser (Set NormalizedUtxo)
parseOgmiosUtxo = withArray "utxo" $ \arr -> do
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

parseCardanoCliUtxo :: Value -> Parser (Set NormalizedUtxo)
parseCardanoCliUtxo = withObject "utxo set" $ \o -> do
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
