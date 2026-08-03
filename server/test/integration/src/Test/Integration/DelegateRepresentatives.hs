{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.DelegateRepresentatives
  ( delegateRepresentativesTests
  ) where

import Data.Aeson (Value(..))
import Data.Aeson.Types (Parser, withArray, withObject)
import Data.Foldable (toList)
import Data.Set (Set)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set

import Test.Integration.Env (TestEnv)
import Test.Integration.Query (assertSameSet, parseIO, queryCli, queryOgmios)

delegateRepresentativesTests :: IO TestEnv -> TestTree
delegateRepresentativesTests getEnv = testGroup "DelegateRepresentatives"
  [ testCase "DRep IDs match cardano-cli" $ do
      env <- getEnv

      ogmiosDreps <- parseIO "ogmios DRep IDs" parseOgmiosDrepIds
        =<< queryOgmios env "queryLedgerState/delegateRepresentatives" Nothing

      cliDreps <- parseIO "cli DRep IDs" parseCliDrepIds
        =<< queryCli env ["conway", "query", "drep-state", "--all-dreps"]

      assertSameSet "DRep ID sets" ogmiosDreps cliDreps
  ]

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

-- Ogmios returns an array of objects with "id" and "type" fields.
-- Special entries "abstain" and "noConfidence" have no "id".
parseOgmiosDrepIds :: Value -> Parser (Set Text)
parseOgmiosDrepIds = withArray "dreps" $ \arr -> do
  ids <- mapM extractId (toList arr)
  pure $ Set.fromList [i | Just i <- ids]
  where
    extractId = withObject "drep" $ \o ->
      case KM.lookup "id" o of
        Just (String drepId) -> pure (Just drepId)
        _ -> pure Nothing

-- cardano-cli drep-state --all-dreps returns an array of [drepId, drepState] pairs
parseCliDrepIds :: Value -> Parser (Set Text)
parseCliDrepIds = withArray "dreps" $ \arr -> do
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
