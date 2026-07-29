{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Shared plumbing for the integration tests: querying ogmios over its
-- WebSocket JSON-RPC interface, querying cardano-cli, and the common
-- parsing/comparison helpers built on top of them.
module Test.Integration.Query
  ( queryOgmios
  , queryCli
  , queryCliStdout
  , assertSameSet
  , parseIO
  ) where

import Control.Monad (when)
import Data.Aeson (Value(..), eitherDecode, encode, object, (.=))
import Data.Aeson.Types (Parser, parseEither)
import Data.Set (Set)
import Data.Text (Text)
import System.IO (hClose, openTempFile)
import System.Process (callProcess, readProcess)
import Test.Tasty.HUnit (assertFailure)

import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.WebSockets as WS

import Test.Integration.Env (TestEnv(..), queryOgmiosRetry)

-- | Send a single JSON-RPC request to ogmios and return its @result@
-- payload. Connection and decoding failures are retried (via
-- 'queryOgmiosRetry'); a JSON-RPC error response fails the test.
queryOgmios :: TestEnv -> Text -> Maybe Value -> IO Value
queryOgmios env method params = do
  resp <- queryOgmiosRetry (envOgmiosPort env) $ \port ->
    WS.runClient "127.0.0.1" port "/" $ \conn -> do
      WS.sendTextData conn $ encode $ object $
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method"  .= method
        , "id"      .= Null
        ] <> foldMap (\p -> [ "params" .= p ]) params
      raw <- WS.receiveData conn
      case eitherDecode raw of
        Left err  -> fail $ "Failed to decode ogmios response: " <> err
        Right val -> pure val
  case resp of
    Object o
      | Just result <- KM.lookup "result" o -> pure result
      | Just err    <- KM.lookup "error"  o ->
          assertFailure $ "Ogmios returned error: " <> show err
    _ -> assertFailure $ "Unexpected ogmios response: " <> show resp

-- | Run a cardano-cli query and decode the JSON it writes to a file. The
-- @--testnet-magic@, @--socket-path@ and @--out-file@ arguments are
-- appended to the given ones. Each call gets a fresh out-file under the
-- work directory (kept around for post-mortem debugging), so parallel
-- tests cannot clobber each other's output.
queryCli :: TestEnv -> [String] -> IO Value
queryCli env args = do
  (outFile, h) <- openTempFile (envWorkDir env) "cli-query.json"
  hClose h
  callProcess "cardano-cli" $ args <>
    [ "--testnet-magic", show (envTestnetMagic env)
    , "--socket-path", envNodeSocket env
    , "--out-file", outFile
    ]
  contents <- LBS.readFile outFile
  case eitherDecode contents of
    Left err  -> fail $ "Failed to decode cardano-cli output: " <> err
    Right val -> pure val

-- | Like 'queryCli', for commands that only emit JSON on stdout and do
-- not support @--out-file@.
queryCliStdout :: TestEnv -> [String] -> IO Value
queryCliStdout env args = do
  output <- readProcess "cardano-cli"
    (args <>
      [ "--testnet-magic", show (envTestnetMagic env)
      , "--socket-path", envNodeSocket env
      ])
    ""
  case eitherDecode (LBS.fromStrict (T.encodeUtf8 (T.pack output))) of
    Left err  -> fail $ "Failed to decode cardano-cli output: " <> err
    Right val -> pure val

-- | Fail the test unless both sides observed the same set, printing the
-- elements only one side has.
assertSameSet :: (Ord a, Show a) => String -> Set a -> Set a -> IO ()
assertSameSet what ogmiosSet cliSet = do
  let ogmiosOnly = Set.difference ogmiosSet cliSet
      cliOnly    = Set.difference cliSet ogmiosSet
  when (not (Set.null ogmiosOnly) || not (Set.null cliOnly)) $
    assertFailure $ unlines
      [ what <> " differ:"
      , "  In Ogmios only (" <> show (Set.size ogmiosOnly) <> "):"
      , concatMap (\e -> "    " <> show e <> "\n") (Set.toList ogmiosOnly)
      , "  In cardano-cli only (" <> show (Set.size cliOnly) <> "):"
      , concatMap (\e -> "    " <> show e <> "\n") (Set.toList cliOnly)
      ]

-- | Run an aeson parser, failing the test on a parse error.
parseIO :: String -> (Value -> Parser a) -> Value -> IO a
parseIO what p val = case parseEither p val of
  Left err -> assertFailure $ "Failed to parse " <> what <> ": " <> err
  Right a  -> pure a
