{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Submitting transactions /through/ ogmios.
--
-- cardano-node's tx-generator can be pointed at a submission endpoint
-- instead of a set of target nodes (@submissionEndpointProtocol: "Ogmios"@,
-- IntersectMBO/cardano-node#6609), in which case every transaction of the
-- run — the initial genesis expenditure included — travels over ogmios's
-- @submitTransaction@ rather than a local node socket. Its final phase pays
-- to the compiler's hardcoded \"BenchmarkingDone\" key, so new UTxOs at that
-- address are the proof that submissions were accepted /and/ made it into
-- blocks.
--
-- Converted from @scripts\/test-txgen-submission.sh@: the binaries the script
-- resolved with nix now come from the @integration@ devshell, and the testnet
-- and ogmios instance are the ones 'Test.Integration.Env' already runs.
module Test.Integration.TxSubmission
  ( txSubmissionTests
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Aeson (Value(..), encode, object, (.=))
import Data.Aeson.Types (withArray, withObject)
import Data.List (isInfixOf)
import Data.Text (Text)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (IOMode(..), withFile)
import System.Process
    ( CreateProcess(cwd, std_err, std_out)
    , StdStream(..)
    , callProcess
    , proc
    , readProcess
    , waitForProcess
    , withCreateProcess
    )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import Test.Integration.Env (TestEnv(..))
import Test.Integration.Query (parseIO, queryCli, queryOgmios)

-- | The signing key the tx-generator's compiler pays to in its last phase.
-- The cborHex must match @keyBenchmarkDone@ in cardano-node's
-- @bench\/tx-generator\/src\/Cardano\/Benchmarking\/Compiler.hs@; its address
-- is derived below rather than hardcoded.
benchmarkDoneSKey :: Value
benchmarkDoneSKey = object
  [ "type" .= ("PaymentSigningKeyShelley_ed25519" :: Text)
  , "description" .= ("" :: Text)
  , "cborHex" .=
      ("582016ca4f13fa17557e56a7d0dd3397d747db8e1e22fdb5b9df638abdb680650d50" :: Text)
  ]

-- | How long to wait for the submitted transactions to show up in blocks.
awaitBlocksSeconds :: Int
awaitBlocksSeconds = 120

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

txSubmissionTests :: IO TestEnv -> TestTree
txSubmissionTests getEnv = testGroup "TxSubmission"
  [ -- This mutates the UTxO set, so it must run after the comparison tests
    -- that assume a quiescent ledger — see the sequencing in "Main".
    testCase "tx-generator submits through ogmios" $ do
      env <- getEnv
      benchAddr <- benchmarkDoneAddress env

      before <- utxoCountAt env benchAddr
      putStrLn $ "[integration] " <> show before <> " UTxOs at "
        <> T.unpack benchAddr <> " before submitting"

      logs <- runTxGeneratorViaOgmios env
      assertSubmittedThroughEndpoint logs

      after <- awaitMoreUtxosAt env benchAddr before
      putStrLn $ "[integration] " <> show after <> " UTxOs at the benchmark "
        <> "address after submitting (" <> show (after - before) <> " new)"
  ]

-- ---------------------------------------------------------------------------
-- Running the generator
-- ---------------------------------------------------------------------------

-- | Run a short tx-generator benchmark whose every phase submits through
-- ogmios, returning its combined output for inspection.
--
-- Unlike the funding run in 'Test.Integration.Env', this one does not use
-- @--testnet-config-dir@: discovery always picks @utxo1@ and overrides the
-- config's own @sigKey@, and that key's genesis fund is already spent by the
-- funding run — a second expenditure of it would be rejected as a missing
-- input. The four connection settings discovery would supply are therefore
-- given explicitly, with a genesis key of our own (cardano-testnet seeds
-- three of them).
runTxGeneratorViaOgmios :: TestEnv -> IO String
runTxGeneratorViaOgmios env = do
  let workDir     = envWorkDir env
      logsDir     = workDir </> "logs"
      configFile  = workDir </> "tx-generator-submission.json"
      sigKeyPath  = envTestnetDir env </> "utxo-keys" </> "utxo2" </> "utxo.skey"
      stdoutPath  = logsDir </> "tx-generator-submission.stdout"
      stderrPath  = logsDir </> "tx-generator-submission.stderr"

  sigKeyExists <- doesFileExist sigKeyPath
  unless sigKeyExists $ assertFailure $
    "Genesis signing key not found: " <> sigKeyPath

  LBS.writeFile configFile $ encode $ object
    [ "tx_count"        .= (10 :: Int)
    , "tps"             .= (2 :: Int)
    , "inputs_per_tx"   .= (2 :: Int)
    , "outputs_per_tx"  .= (2 :: Int)
    , "tx_fee"          .= (212345 :: Int)
    , "min_utxo_value"  .= (1000000 :: Int)
    , "add_tx_size"     .= (39 :: Int)
    , "init_cooldown"   .= (5 :: Int)
    , "era"             .= ("Conway" :: Text)
    , "keepalive"       .= (30 :: Int)
    , "debugMode"       .= False
    , "plutus"          .= Null
      -- connection settings, see the note above
    , "sigKey"              .= sigKeyPath
    , "localNodeSocketPath" .= envNodeSocket env
    , "nodeConfigFile"      .= envNodeConfig env
      -- an endpoint replaces the target nodes as the submission target, and
      -- the compiler rejects a config providing both. Spelling it out also
      -- keeps a tx-generator without endpoint support (which ignores the two
      -- keys below) from silently benchmarking node-to-node instead.
    , "targetNodes"    .= ([] :: [Value])
    , "submissionEndpointProtocol" .= ("Ogmios" :: Text)
    , "submissionEndpointURI"      .=
        ("ws://127.0.0.1:" <> show (envOgmiosPort env))
    ]

  putStrLn "[integration] running tx-generator with ogmios submission..."
  exitCode <-
    withFile stdoutPath WriteMode $ \txgenStdout ->
    withFile stderrPath WriteMode $ \txgenStderr ->
      withCreateProcess
        (proc "tx-generator" [ "json_highlevel", configFile ])
        { cwd = Just workDir
        , std_out = UseHandle txgenStdout
        , std_err = UseHandle txgenStderr
        }
        $ \_ _ _ ph -> waitForProcess ph

  out <- readFile stdoutPath
  err <- readFile stderrPath
  case exitCode of
    ExitSuccess -> putStrLn "[integration] tx-generator finished."
    ExitFailure c -> assertFailure $ unlines $
      [ "tx-generator exited with code " <> show c <> "." ]
        <> endpointSupportHint (out <> err) <> logTails out err
  pure (out <> err)

-- | A tx-generator predating IntersectMBO/cardano-node#6609 parses
-- @targetNodes@ as a non-empty list, having no submission endpoint to replace
-- them with, so it dies on the empty one above. Name that cause: it is the
-- likely one, and its own diagnostic does not mention ogmios at all.
endpointSupportHint :: String -> [String]
endpointSupportHint logs
  | "parsing NonEmpty failed" `isInfixOf` logs =
      [ "That is what a tx-generator without Ogmios submission support makes"
      , "of the empty targetNodes this test configures. It has to come from a"
      , "cardano-node carrying IntersectMBO/cardano-node#6609: see the"
      , "cardano-node-tx-generator flake input."
      ]
  | otherwise = []

-- | Guard against a false pass: a tx-generator predating
-- IntersectMBO/cardano-node#6609 ignores the two @submissionEndpoint*@ keys,
-- and would submit through a local node socket without ogmios ever seeing a
-- transaction. Such a version fails on the empty @targetNodes@ above, but
-- check its own report of the submission target too.
assertSubmittedThroughEndpoint :: String -> IO ()
assertSubmittedThroughEndpoint logs =
  unless (any (`isInfixOf` logs) evidence) $ assertFailure $ unlines $
    [ "tx-generator did not report submitting through an endpoint, so this run"
    , "proves nothing about ogmios: a tx-generator without Ogmios submission"
    , "support ignores the submissionEndpoint* configuration keys."
    , "Expected its output to mention one of:"
    ] <> map ("  " <>) evidence <> [ "", "tx-generator output:", lastLines 30 logs ]
  where
    evidence =
      [ "_nix_submissionEndpointProtocol = Just Ogmios"
      , "submits through an endpoint"
      ]

-- ---------------------------------------------------------------------------
-- Counting UTxOs at the benchmark address
-- ---------------------------------------------------------------------------

-- | Derive the address the benchmark's last phase pays to, the same way the
-- generator does: from 'benchmarkDoneSKey'.
benchmarkDoneAddress :: TestEnv -> IO Text
benchmarkDoneAddress env = do
  let skeyFile = envWorkDir env </> "benchmark-done.skey"
      vkeyFile = envWorkDir env </> "benchmark-done.vkey"
  LBS.writeFile skeyFile (encode benchmarkDoneSKey)
  callProcess "cardano-cli"
    [ "key", "verification-key"
    , "--signing-key-file", skeyFile
    , "--verification-key-file", vkeyFile
    ]
  addr <- readProcess "cardano-cli"
    [ "address", "build"
    , "--payment-verification-key-file", vkeyFile
    , "--testnet-magic", show (envTestnetMagic env)
    ] ""
  pure (T.strip (T.pack addr))

-- | How many UTxOs sit at an address, as agreed by ogmios and cardano-cli.
-- Disagreement fails the test: an address-filtered query is not covered by
-- the whole-UTxO comparison in "Test.Integration.Utxo".
utxoCountAt :: TestEnv -> Text -> IO Int
utxoCountAt env addr = do
  ogmiosCount <- parseIO "ogmios UTxO" (withArray "utxo" (pure . length))
    =<< queryOgmios env "queryLedgerState/utxo"
          (Just (object [ "addresses" .= [addr] ]))

  cliCount <- parseIO "cardano-cli UTxO" (withObject "utxo set" (pure . KM.size))
    =<< queryCli env [ "conway", "query", "utxo", "--address", T.unpack addr ]

  unless (ogmiosCount == cliCount) $ assertFailure $ unlines
    [ "UTxO counts at " <> T.unpack addr <> " differ:"
    , "  ogmios:      " <> show ogmiosCount
    , "  cardano-cli: " <> show cliCount
    ]
  pure ogmiosCount

-- | Wait for the submitted transactions to be included in blocks, i.e. for
-- new UTxOs to appear at the benchmark address. Once one shows up, settle
-- briefly and report the final count.
awaitMoreUtxosAt :: TestEnv -> Text -> Int -> IO Int
awaitMoreUtxosAt env addr before = go awaitBlocksSeconds
  where
    go n
      | n <= 0 = assertFailure $ unlines
          [ "No new UTxOs appeared at " <> T.unpack addr <> " within "
            <> show awaitBlocksSeconds <> "s."
          , "tx-generator reported every submission as accepted, so the"
          , "transactions should have made it into blocks by now."
          , "  UTxOs before: " <> show before
          ]
      | otherwise = do
          count <- utxoCountAt env addr
          if count > before
            then threadDelay 5000000 >> utxoCountAt env addr
            else threadDelay 2000000 >> go (n - 2)

-- ---------------------------------------------------------------------------
-- Reporting
-- ---------------------------------------------------------------------------

-- | The tail of both logs, for a failure message. Worth spelling out: a
-- tx-generator that fails before its tracer is initialized can die without
-- diagnostics of its own (see the recursion in cardano-node's
-- @Cardano.Benchmarking.Script.Env.getBenchTracers@).
logTails :: String -> String -> [String]
logTails out err =
  [ "", "tx-generator stdout (last 30 lines):", lastLines 30 out
  , "", "tx-generator stderr (last 20 lines):", lastLines 20 err
  ]

lastLines :: Int -> String -> String
lastLines n = unlines . reverse . take n . reverse . lines
