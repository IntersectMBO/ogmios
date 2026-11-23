-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TemplateHaskell #-}

-- Used to partially pattern match result of parsing default arguments. Okay-ish
-- because it's test code and, having it fail would be instantly caught.
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Ogmios.OptionsSpec
    ( spec
    ) where

import Ogmios.Prelude

import Data.Aeson
    ( (.=)
    )
import Data.Aeson.Lens
    ( key
    )
import Data.List
    ( isInfixOf
    )
import Ogmios.App.Configuration
    ( Configuration (..)
    , EpochSlots (..)
    , IncludeCbor (..)
    , NetworkMagic (..)
    , NetworkParameters (..)
    , includeAllCbor
    , lookupNodeConfig
    , mkSystemStart
    , readAlonzoGenesis
    , readByronGenesis
    , readConwayGenesis
    , readDijkstraGenesis
    , readShelleyGenesis
    )
import Ogmios.Control.MonadLog
    ( Severity (..)
    , TracerDefinition (..)
    , defaultTracers
    )
import Ogmios.Data.Json.Prelude
    ( inefficientEncodingToValue
    )
import Ogmios.Options
    ( Command (..)
    , Tracers (..)
    , parseNetworkParameters
    , parseOptions
    , parseOptionsPure
    )
import System.Environment
    ( withArgs
    )
import Test.Hspec
    ( Expectation
    , Spec
    , context
    , expectationFailure
    , parallel
    , shouldBe
    , shouldSatisfy
    , specify
    )
import Test.Path.Util
    ( getProjectRoot
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as T
import qualified Ogmios.Data.Json.Dijkstra as Dijkstra

spec :: Spec
spec = parallel $ do
    context "parseOptions(Pure)" $ do
        forM_ matrix $ \(args, expect) -> do
            let title = toString $ unwords $ toText <$> args
            specify title $ expect (parseOptionsPure args)

        specify "invalid" $ do
            case parseOptionsPure ["--nope"] of
                Right{} -> expectationFailure "Expected error but got success."
                Left e -> e `shouldSatisfy` isInfixOf "Invalid option"

        specify "test completion" $ do
            case parseOptionsPure ["--node-so\t"] of
                Right{} -> expectationFailure "Expected error but got success."
                Left e -> e `shouldSatisfy` isInfixOf "Invalid option"

    context "parseOptions(IO)" $ do
        specify "--version" $ withArgs ["--version"] parseOptions >>= \case
            Version -> pure ()
            Start{} -> expectationFailure "Expected Version but got Start."
            HealthCheck{} -> expectationFailure "Expected Version but got HealthCheck."
            Inspect{} -> expectationFailure "Expected Version but got Inspect."

        let args =
                [ "--node-socket", "./node.socket"
                , "--node-config", getConfigFile "preview"
                ]
        specify (show args) $ withArgs args parseOptions >>= \case
            Start (Identity _) opts logLevels -> do
                nodeSocket opts `shouldBe` "./node.socket"
                nodeConfig opts `shouldBe` getConfigFile "preview"
                serverHost opts `shouldBe` "127.0.0.1"
                serverPort opts `shouldBe` 1337
                connectionTimeout opts `shouldBe` 90
                maxInFlight opts `shouldBe` 1000
                logLevels `shouldBe` defaultTracersInfo
            Version -> expectationFailure "Expected Start but got Version."
            HealthCheck{} -> expectationFailure "Expected Start but got HealthCheck."
            Inspect{} -> expectationFailure "Expected Start but got Inspect."

    context "parseNetworkParameters" $ do
        specify "mainnet" $ do
            params <- parseNetworkParameters (getConfigFile "mainnet")
            networkMagic  params `shouldBe` NetworkMagic 764824073
            systemStart   params `shouldBe` mkSystemStart 1506203091
            slotsPerEpoch params `shouldBe` EpochSlots 21600

        specify "preview" $ do
            params <- parseNetworkParameters (getConfigFile "preview")
            networkMagic  params `shouldBe` NetworkMagic 2
            systemStart   params `shouldBe` mkSystemStart 1666656000
            slotsPerEpoch params `shouldBe` EpochSlots 4320

        specify "preprod" $ do
            params <- parseNetworkParameters (getConfigFile "preprod")
            networkMagic  params `shouldBe` NetworkMagic 1
            systemStart   params `shouldBe` mkSystemStart 1654041600
            slotsPerEpoch params `shouldBe` EpochSlots 21600

        specify "cardano-testnet 11.2.0 (settings in sections)" $ do
            params <- parseNetworkParameters sectionedConfigFile
            networkMagic  params `shouldBe` NetworkMagic 42
            systemStart   params `shouldBe` mkSystemStart 1795464068
            slotsPerEpoch params `shouldBe` EpochSlots 50

    context "lookupNodeConfig" $ do
        let lookupShelley = lookupNodeConfig "ShelleyGenesisFile"
        let configuration = ("Configuration" .=) . Json.object
        let protocol = ("Protocol" .=) . Json.object
        let shelley = ("ShelleyGenesisFile" .=) . Json.String

        specify "at the top level" $
            lookupShelley (Json.object [shelley "top"])
                `shouldBe` Just "top"

        specify "in the Protocol section" $
            lookupShelley (Json.object [configuration [protocol [shelley "protocol"]]])
                `shouldBe` Just "protocol"

        specify "the Dijkstra genesis in the Testing section" $
            lookupNodeConfig "DijkstraGenesisFile"
                (Json.object [configuration ["Testing" .= Json.object ["DijkstraGenesisFile" .= Json.String "testing"]]])
                `shouldBe` Just "testing"

        specify "the Dijkstra genesis not in the Protocol section" $
            lookupNodeConfig "DijkstraGenesisFile"
                (Json.object [configuration [protocol ["DijkstraGenesisFile" .= Json.String "protocol"]]])
                `shouldBe` Nothing

        specify "not in the Testing section" $
            lookupShelley (Json.object [configuration ["Testing" .= Json.object [shelley "testing"]]])
                `shouldBe` Nothing

        specify "loose under Configuration" $
            lookupShelley (Json.object [configuration [shelley "loose"]])
                `shouldBe` Just "loose"

        specify "next to Configuration" $
            lookupShelley (Json.object [configuration [], shelley "top"])
                `shouldBe` Just "top"

        specify "the Protocol section wins" $
            lookupShelley (Json.object
                [ configuration
                    [ "Network" .= Json.object [shelley "network"]
                    , protocol [shelley "protocol"]
                    , shelley "loose"
                    ]
                , shelley "top"
                ])
                `shouldBe` Just "protocol"

        specify "not in other sections" $
            lookupShelley (Json.object [configuration ["Network" .= Json.object [shelley "network"]]])
                `shouldBe` Nothing

    context "genesis configurations" $ do
        forM_ [ ("mainnet (flat settings)", getConfigFile "mainnet")
              , ("cardano-testnet 11.2.0 (settings in sections)", sectionedConfigFile)
              ] $ \(name, configFile) -> specify name $ do
            void $ readByronGenesis configFile
            void $ readShelleyGenesis configFile
            void $ readAlonzoGenesis configFile
            readConwayGenesis configFile >>= (`shouldSatisfy` isRight)

        specify "Dijkstra (cardano-testnet 11.2.0)" $ do
            genesis <- readDijkstraGenesis sectionedConfigFile >>= either (fail . toString) pure
            let parameters = inefficientEncodingToValue (Dijkstra.encodeGenesis genesis) ^? key "updatableParameters"
            file <- Json.eitherDecodeFileStrict @Json.Object sectionedDijkstraGenesisFile >>= either fail pure
            -- Every field of the genesis file, and where Ogmios puts it.
            sort (fst <$> fromOgmiosDijkstraGenesis) `shouldBe` sort (KeyMap.keys file)
            forM_ fromOgmiosDijkstraGenesis $ \(name, fromOgmios) ->
                (name, asNumbers <$> (parameters >>= fromOgmios))
                    `shouldBe` (name, asNumbers <$> KeyMap.lookup name file)
  where
    matrix =
        [ ( [], shouldFail )
        , ( [ "--node-socket", "./node.socket" ], shouldFail )
        , ( [ "--node-config", "./node.config" ], shouldFail )

        , ( [ "--node-socket", "/path/to/socket"
            , "--node-config", "./node.config"
            ]
          , shouldSucceed
                (defaultConfiguration { nodeSocket = "/path/to/socket" })
                defaultTracersInfo
          )

        , ( [ "--node-socket", "./node.socket"
            , "--node-config", "/path/to/config"
            ]
          , shouldSucceed
                (defaultConfiguration { nodeConfig = "/path/to/config" })
                defaultTracersInfo
          )

        , ( defaultArgs ++ [ "--host", "0.0.0.0" ]
          , shouldSucceed
                (defaultConfiguration { serverHost = "0.0.0.0" })
                defaultTracersInfo
          )

        , ( defaultArgs ++ [ "--port", "42" ]
          , shouldSucceed
                (defaultConfiguration { serverPort = 42 })
                defaultTracersInfo
          )
        , ( defaultArgs ++ [ "--port", "#" ]
          , shouldFail
          )

        , ( defaultArgs ++ [ "--include-cbor" ]
          , shouldSucceed
                (defaultConfiguration { includeCbor = includeAllCbor })
                defaultTracersInfo
          )
        , ( defaultArgs ++ [ "--include-transaction-cbor" ]
          , shouldSucceed
                (defaultConfiguration { includeCbor = IncludeCbor True False False })
                defaultTracersInfo
          )
        , ( defaultArgs ++ [ "--include-cbor", "--include-transaction-cbor" ]
          , shouldFail
          )

        , ( defaultArgs ++ [ "--timeout", "42" ]
          , shouldSucceed
                (defaultConfiguration { connectionTimeout = 42 })
                defaultTracersInfo
          )
        , ( defaultArgs ++ [ "--timeout", "#" ]
          , shouldFail
          )

        , ( defaultArgs ++ [ "--max-in-flight", "42" ]
          , shouldSucceed
                (defaultConfiguration { maxInFlight = 42 })
                defaultTracersInfo
          )
        , ( defaultArgs ++ [ "--max-in-flight", "#" ]
          , shouldFail
          )

        , ( defaultArgs ++ [ "--log-level", "Debug" ]
          , shouldSucceed defaultConfiguration defaultTracersDebug
          )
        , ( defaultArgs ++ [ "--log-level", "debug" ]
          , shouldSucceed defaultConfiguration defaultTracersDebug
          )
        , ( defaultArgs ++ [ "--log-level", "Info" ]
          , shouldSucceed defaultConfiguration defaultTracersInfo
          )
        , ( defaultArgs ++ [ "--log-level", "info" ]
          , shouldSucceed defaultConfiguration defaultTracersInfo
          )
        , ( defaultArgs ++ [ "--log-level", "Notice" ]
          , shouldSucceed defaultConfiguration defaultTracersNotice
          )
        , ( defaultArgs ++ [ "--log-level", "notice" ]
          , shouldSucceed defaultConfiguration defaultTracersNotice
          )
        , ( defaultArgs ++ [ "--log-level", "Warning" ]
          , shouldSucceed defaultConfiguration defaultTracersWarning
          )
        , ( defaultArgs ++ [ "--log-level", "warning" ]
          , shouldSucceed defaultConfiguration defaultTracersWarning
          )
        , ( defaultArgs ++ [ "--log-level", "Error" ]
          , shouldSucceed defaultConfiguration defaultTracersError
          )
        , ( defaultArgs ++ [ "--log-level", "error" ]
          , shouldSucceed defaultConfiguration defaultTracersError
          )

        , ( defaultArgs ++ [ "--log-level-health", "Notice" ]
          , shouldSucceed
                defaultConfiguration
                (defaultTracersInfo { tracerHealth = Const (Just Notice) })
          )

        , ( defaultArgs ++
                [ "--log-level-metrics", "Debug"
                , "--log-level-websocket", "Warning"
                ]
          , shouldSucceed
                defaultConfiguration
                (defaultTracersInfo
                    { tracerMetrics = Const (Just Debug)
                    , tracerWebSocket = Const (Just Warning)
                    }
                )
          )

        , ( defaultArgs ++
                [ "--log-level", "Error"
                , "--log-level-health", "Debug"
                ]
          , shouldFail
          )

        , ( [ "version" ], flip shouldBe $ Right Version )
        , ( [ "-v" ], flip shouldBe $ Right Version )
        , ( [ "--version" ], flip shouldBe $ Right Version )

        , ( [ "--help" ]
          , flip shouldSatisfy $ isLeftWith $ \help ->
            help `deepseq` ("Usage:" `isInfixOf` help)
          )
        , ( [ "-h" ]
          , flip shouldSatisfy $ isLeftWith $ \help ->
            help `deepseq` ("Usage:" `isInfixOf` help)
          )
        ]

--
-- Helper
--

defaultConfiguration :: Configuration
defaultConfiguration = parseOptionsPure defaultArgs
    & either (error . toText) (\(Start _ cfg _) -> cfg)

defaultTracersDebug :: Tracers IO 'MinSeverities
defaultTracersDebug = defaultTracers (Just Debug)

defaultTracersInfo :: Tracers IO 'MinSeverities
defaultTracersInfo = defaultTracers (Just Info)

defaultTracersNotice :: Tracers IO 'MinSeverities
defaultTracersNotice = defaultTracers (Just Notice)

defaultTracersWarning :: Tracers IO 'MinSeverities
defaultTracersWarning = defaultTracers (Just Warning)

defaultTracersError :: Tracers IO 'MinSeverities
defaultTracersError = defaultTracers (Just Error)

defaultArgs :: [String]
defaultArgs =
    [ "--node-socket", "./node.socket"
    , "--node-config", "./node.config"
    ]

shouldSucceed
    :: Configuration
    -> Tracers IO 'MinSeverities
    -> (Either String (Command Proxy) -> Expectation)
shouldSucceed cfg =
    flip shouldBe . Right . Start Proxy cfg

shouldFail :: Either String (Command Proxy) -> Expectation
shouldFail = flip shouldSatisfy isLeft

isLeftWith :: (err -> Bool) -> Either err result -> Bool
isLeftWith predicate = \case
    Left e -> predicate e
    Right{} -> False

fromOgmiosDijkstraGenesis :: [(Json.Key, Json.Value -> Maybe Json.Value)]
fromOgmiosDijkstraGenesis =
    [ ("maxRefScriptSizePerBlock", (^? key "maxReferenceScriptsSizePerBlock" . key "bytes"))
    , ("maxRefScriptSizePerTx", (^? key "maxReferenceScriptsSizePerTransaction" . key "bytes"))
    , ("refScriptCostStride", (^? key "minFeeReferenceScripts" . key "range"))
    , ("refScriptCostMultiplier", (^? key "minFeeReferenceScripts" . key "multiplier"))
    , ("maxPledgeLeverage", (^? key "maxStakePoolPledgeLeverage"))
    , ("minPoolMargin", (^? key "minStakePoolMargin"))
    , ("plutusV4CostModel", (^? key "plutusCostModels" . key "plutus:v4"))
    , ("leiosAnnouncementPeriodLength", (^? key "leiosAnnouncementPeriodLength" . key "milliseconds"))
    , ("leiosVotePeriodLength", (^? key "leiosVotePeriodLength" . key "milliseconds"))
    , ("leiosDiffusionPeriodLength", (^? key "leiosDiffusionPeriodLength" . key "milliseconds"))
    , ("leiosCommitteeSize", (^? key "leiosCommitteeSize"))
    , ("leiosQuorumStakeThreshold", (^? key "leiosQuorumStakeThreshold"))
    , ("maxEndorserBlockReferencesSize", (^? key "maxEndorserBlockReferencesSize" . key "bytes"))
    , ("maxEndorserBlockTxsSize", (^? key "maxEndorserBlockTransactionsSize" . key "bytes"))
    , ("maxEndorserBlockExecutionUnits", \p -> do
        units <- p ^? key "maxExecutionUnitsPerEndorserBlock"
        memory <- units ^? key "memory"
        steps <- units ^? key "cpu"
        pure $ Json.object [ "memory" .= memory, "steps" .= steps ])
    , ("maxRefScriptSizePerEndorserBlock", (^? key "maxReferenceScriptsSizePerEndorserBlock" . key "bytes"))
    , ("perasMinCandidateBlockAge", (^? key "perasMinCandidateBlockAge"))
    , ("perasHealingFactor", (^? key "perasHealingFactor"))
    , ("perasCertBoost", (^? key "perasCertificateBoost"))
    , ("perasTargetCommitteeSize", (^? key "perasTargetCommitteeSize"))
    , ("perasBootstrapRound", (^? key "perasBootstrapRound"))
    , ("perasQuorumThresholdSafetyMargin", (^? key "perasQuorumThresholdSafetyMargin"))
    , ("refInputsCostPerMultiAssetPolicy", (^? key "referenceInputsCostPerMultiAssetPolicy" . key "ada" . key "lovelace"))
    , ("refInputsCostPerDatumByte", (^? key "referenceInputsCostPerDatumByte"))
    ]

-- | The genesis file has plain numbers, where Ogmios has ratios such as "3/4".
asNumbers :: Json.Value -> Json.Value
asNumbers = \case
    Json.Number n ->
        Json.toJSON (realToFrac n :: Double)
    Json.String (T.splitOn "/" -> [numerator, denominator])
        | Just n <- readMaybe @Double (toString numerator)
        , Just d <- readMaybe @Double (toString denominator) ->
            Json.toJSON (n / d)
    Json.Array xs ->
        Json.Array (asNumbers <$> xs)
    Json.Object o ->
        Json.Object (asNumbers <$> o)
    other ->
        other

sectionedConfigFile :: FilePath
sectionedConfigFile =
    $(getProjectRoot) <> "/test/node-config/cardano-testnet-11.2.0/configuration.yaml"

sectionedDijkstraGenesisFile :: FilePath
sectionedDijkstraGenesisFile =
    $(getProjectRoot) <> "/test/node-config/cardano-testnet-11.2.0/dijkstra-genesis.json"

getConfigFile :: String -> FilePath
getConfigFile network =
    $(getProjectRoot) <> "/config/network/" <> network <> "/cardano-node/config.json"
