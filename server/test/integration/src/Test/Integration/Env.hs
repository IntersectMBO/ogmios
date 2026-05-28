{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Env
  ( TestEnv(..)
  , withTestEnv
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (filterM)
import Data.Foldable (toList)
import Data.Maybe (catMaybes, listToMaybe)
import Network.Socket
    ( AddrInfo(..)
    , SocketType(..)
    , close
    , connect
    , defaultHints
    , getAddrInfo
    , openSocket
    )
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , findExecutable
    , listDirectory
    )
import System.Environment (getEnvironment)
import System.FilePath ((</>), takeFileName)
import System.IO (Handle, IOMode(..), hClose, openFile)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(..)
    , createProcess
    , proc
    , terminateProcess
    , waitForProcess
    )
import Test.Tasty (TestTree, withResource)

data TestEnv = TestEnv
  { envWorkDir      :: !FilePath
  , envNodeSocket   :: !FilePath
  , envNodeConfig   :: !FilePath
  , envOgmiosPort   :: !Int
  , envTestnetMagic :: !Int
  }

data ManagedState = ManagedState
  { msProcesses  :: [ProcessHandle]
  , msLogHandles :: [Handle]
  , msWorkDir    :: FilePath
  }

ogmiosPort :: Int
ogmiosPort = 11337

testnetMagic :: Int
testnetMagic = 42

withTestEnv :: (IO TestEnv -> TestTree) -> TestTree
withTestEnv f = withResource setup' teardown' (f . fmap fst)
  where
    setup' = setup
    teardown' (_, managed) = teardown managed

setup :: IO (TestEnv, ManagedState)
setup = do
  tmpBase <- getCanonicalTemporaryDirectory
  workDir <- createTempDirectory tmpBase "ogmios-integration"
  let logsDir    = workDir </> "logs"
      testnetDir = workDir </> "testnet"
  createDirectoryIfMissing True logsDir

  putStrLn $ "[integration] work directory: " <> workDir

  -- Resolve tool paths for cardano-testnet's env vars
  toolEnv <- resolveToolEnv

  -- Start cardano-testnet
  testnetStdout <- openFile (logsDir </> "cardano-testnet.stdout") WriteMode
  testnetStderr <- openFile (logsDir </> "cardano-testnet.stderr") WriteMode
  (_, _, _, testnetPh) <- createProcess
    (proc "cardano-testnet"
      [ "cardano"
      , "--testnet-magic", show testnetMagic
      , "--output-dir", testnetDir
      ])
    { env = Just toolEnv
    , std_out = UseHandle testnetStdout
    , std_err = UseHandle testnetStderr
    }

  putStrLn "[integration] waiting for cardano-testnet node socket..."
  socketPath <- waitForFile testnetDir ["sock"] 120
  putStrLn $ "[integration] found node socket: " <> socketPath

  configPath <- findNodeConfig testnetDir
  putStrLn $ "[integration] found node config: " <> configPath

  -- Start ogmios
  ogmiosStdout <- openFile (logsDir </> "ogmios.stdout") WriteMode
  ogmiosStderr <- openFile (logsDir </> "ogmios.stderr") WriteMode
  (_, _, _, ogmiosPh) <- createProcess
    (proc "ogmios"
      [ "--node-socket", socketPath
      , "--node-config", configPath
      , "--port", show ogmiosPort
      , "--log-level", "error"
      ])
    { std_out = UseHandle ogmiosStdout
    , std_err = UseHandle ogmiosStderr
    }

  putStrLn "[integration] waiting for ogmios..."
  waitForTcpPort ogmiosPort 60
  putStrLn "[integration] ogmios is ready."

  let testEnv = TestEnv
        { envWorkDir      = workDir
        , envNodeSocket   = socketPath
        , envNodeConfig   = configPath
        , envOgmiosPort   = ogmiosPort
        , envTestnetMagic = testnetMagic
        }
      managed = ManagedState
        { msProcesses  = [ogmiosPh, testnetPh]
        , msLogHandles = [testnetStdout, testnetStderr, ogmiosStdout, ogmiosStderr]
        , msWorkDir    = workDir
        }
  pure (testEnv, managed)

teardown :: ManagedState -> IO ()
teardown ms = do
  mapM_ (\ph -> terminateProcess ph >> waitForProcess ph) (msProcesses ms)
  mapM_ hClose (msLogHandles ms)
  putStrLn $ "[integration] logs available at: " <> msWorkDir ms </> "logs"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- cardano-testnet needs CARDANO_CLI and CARDANO_NODE env vars
-- pointing at the executables (it won't find them via PATH alone).
resolveToolEnv :: IO [(String, String)]
resolveToolEnv = do
  cliPath  <- requireExe "cardano-cli"
  nodePath <- requireExe "cardano-node"
  baseEnv  <- getEnvironment
  pure $ baseEnv
    <> [ ("CARDANO_CLI",  cliPath)
       , ("CARDANO_NODE", nodePath)
       ]
  where
    requireExe name = do
      mPath <- findExecutable name
      case mPath of
        Just p  -> pure p
        Nothing -> fail $ name <> " not found on PATH"

waitForFile :: FilePath -> [String] -> Int -> IO FilePath
waitForFile dir names maxSeconds = go maxSeconds
  where
    go 0 = fail $
      "File(s) " <> show names <> " not found under " <> dir
      <> " after " <> show maxSeconds <> "s"
    go n = do
      result <- tryNames names
      case result of
        Just path -> pure path
        Nothing   -> threadDelay 1000000 >> go (n - 1)

    tryNames [] = pure Nothing
    tryNames (name:rest) = do
      result <- findRecursive dir name
      case result of
        Just path -> pure (Just path)
        Nothing   -> tryNames rest

findRecursive :: FilePath -> String -> IO (Maybe FilePath)
findRecursive dir name = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure Nothing
    else do
      entries <- listDirectory dir
      let fullPaths = map (dir </>) entries
      files <- filterM doesFileExist fullPaths
      case filter (\p -> takeFileName p == name) files of
        (f:_) -> pure (Just f)
        []    -> do
          dirs <- filterM doesDirectoryExist fullPaths
          results <- mapM (\d -> findRecursive d name) dirs
          pure . listToMaybe . catMaybes $ toList results

findNodeConfig :: FilePath -> IO FilePath
findNodeConfig dir = tryNames configNames
  where
    configNames = ["configuration.yaml", "configuration.json", "config.json"]

    tryNames [] = fail $
      "No node configuration found under " <> dir
      <> " (tried: " <> show configNames <> ")"
    tryNames (name:rest) = do
      result <- findRecursive dir name
      case result of
        Just path -> pure path
        Nothing   -> tryNames rest

waitForTcpPort :: Int -> Int -> IO ()
waitForTcpPort port maxSeconds = go maxSeconds
  where
    go 0 = fail $
      "Port " <> show port <> " not accepting connections after "
      <> show maxSeconds <> "s"
    go n = do
      result <- try @SomeException $ do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show port))
        sock <- openSocket addr
        connect sock (addrAddress addr)
        close sock
      case result of
        Right () -> pure ()
        Left _   -> threadDelay 1000000 >> go (n - 1)
