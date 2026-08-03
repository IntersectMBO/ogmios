{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Integration.LedgerTip
  ( ledgerTipTests
  ) where

import Data.Aeson (Value, (.:))
import Data.Aeson.Key (Key)
import Data.Aeson.Types (withObject)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Test.Integration.Env (TestEnv)
import Test.Integration.Query (parseIO, queryCli, queryOgmios)

ledgerTipTests :: IO TestEnv -> TestTree
ledgerTipTests getEnv = testGroup "LedgerTip"
  [ testCase "ledgerState/tip slot is bracketed by cardano-cli tip" $ do
      env <- getEnv

      cliBefore <- queryCli env ["conway", "query", "tip"]
      cSlotBefore <- parseField cliBefore "slot"

      ogmiosTip <- queryOgmios env "queryLedgerState/tip" Nothing
      oSlot <- parseField ogmiosTip "slot"

      cliAfter <- queryCli env ["conway", "query", "tip"]
      cSlotAfter <- parseField cliAfter "slot"

      assertBool
        ("Expected cli_before <= ogmios <= cli_after, got: "
         <> show cSlotBefore <> " <= " <> show oSlot <> " <= " <> show cSlotAfter)
        (cSlotBefore <= oSlot && oSlot <= cSlotAfter)
  ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

parseField :: Value -> Key -> IO Integer
parseField val key = parseIO ("field " <> show key) (withObject "obj" (.: key)) val
