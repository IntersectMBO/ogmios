--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE QuasiQuotes #-}

module Ogmios.Data.Ledger.PredicateFailureSpec
    ( spec
    ) where

import Ogmios.Prelude

import Cardano.Ledger.Address
    ( AccountAddress (..)
    , AccountId (..)
    , Withdrawals (..)
    )
import Cardano.Ledger.BaseTypes
    ( EpochNo (..)
    , Mismatch (..)
    , Network (..)
    )
import Cardano.Ledger.Coin
    ( Coin (..)
    )
import Cardano.Ledger.Conway
    ( ApplyTxError (ConwayApplyTxError)
    )
import Cardano.Ledger.Credential
    ( Credential (..)
    )
import Cardano.Ledger.Dijkstra
    ( ApplyTxError (DijkstraApplyTxError)
    )
import Cardano.Ledger.Hashes
    ( VRFVerKeyHash (..)
    )
import Cardano.Ledger.Keys
    ( KeyHash (..)
    , KeyRole (..)
    )
import Cardano.Network.Protocol.NodeToClient
    ( Block
    , SubmitTransactionError
    )
import Data.Aeson
    ( (.=)
    )
import Data.Aeson.QQ.Simple
    ( aesonQQ
    )
import Ogmios.Data.Json
    ( Json
    , encodeSubmitTransactionError
    , inefficientEncodingToValue
    )
import Ouroboros.Consensus.Cardano.Block
    ( HardForkApplyTxErr (..)
    )
import Test.Hspec
    ( Spec
    , context
    , shouldBe
    , specify
    )

import qualified Cardano.Ledger.Conway.Rules as Cn
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Dijkstra.Rules as Di
import qualified Cardano.Ledger.Shelley.Rules as Sh
import qualified Codec.Json.Rpc as Rpc
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Set.NonEmpty as NESet

spec :: Spec
spec = do
    let inConway = ApplyTxErrConway . ConwayApplyTxError
    let inDijkstra = ApplyTxErrDijkstra . DijkstraApplyTxError . fmap Di.LedgerFailure

    context "Shelley POOL failures, through Conway" $ do
        specify "retirement in the current epoch" $ do
            let failure = Sh.StakePoolRetirementWrongEpochPOOL retiringTooEarly retiringTooEarlyLimit
            encodeSubmissionError (inLedger inConway failure)
                `shouldBe` retiringTooEarlyError

        specify "retirement one epoch after the limit" $ do
            let failure = Sh.StakePoolRetirementWrongEpochPOOL retiringTooLate retiringTooLateLimit
            encodeSubmissionError (inLedger inConway failure)
                `shouldBe` retiringTooLateError

        specify "VRF key of another stake pool" $ do
            let failure = Sh.VRFKeyHashAlreadyRegistered poolId vrfKeyHash
            encodeSubmissionError (inLedger inConway failure)
                `shouldBe` vrfKeyAlreadyRegisteredError

    context "Conway withdrawals" $ do
        specify "partial withdrawal" $ do
            let failure = Cn.ConwayIncompleteWithdrawals (NEMap.singleton accountA partialWithdrawal)
            encodeSubmissionError (inConway $ pure failure)
                `shouldBe` partialWithdrawalError

    context "Dijkstra POOL failures" $ do
        specify "retirement in the current epoch" $ do
            let failure = Di.StakePoolRetirementWrongEpochPOOL retiringTooEarly retiringTooEarlyLimit
            encodeSubmissionError (inLedger inDijkstra failure)
                `shouldBe` retiringTooEarlyError

        specify "retirement one epoch after the limit" $ do
            let failure = Di.StakePoolRetirementWrongEpochPOOL retiringTooLate retiringTooLateLimit
            encodeSubmissionError (inLedger inDijkstra failure)
                `shouldBe` retiringTooLateError

        specify "VRF key of another stake pool" $ do
            let failure = Di.VRFKeyHashAlreadyRegistered poolId vrfKeyHash
            encodeSubmissionError (inLedger inDijkstra failure)
                `shouldBe` vrfKeyAlreadyRegisteredError

    context "Dijkstra withdrawals" $ do
        specify "missing account" $ do
            let failure = Di.WithdrawalAccountsMissing withdrawalsFromUnknownAccounts
            encodeSubmissionError (inLedger inDijkstra failure)
                `shouldBe` unknownCredentialError

        specify "account missing at the start of the batch" $ do
            let failure = Di.WithdrawalAccountsMissingFromOriginal withdrawalsFromUnknownAccounts
            encodeSubmissionError (inLedger inDijkstra failure)
                `shouldBe` unknownCredentialError

        specify "missing account in a sub-transaction" $ do
            let failure = Di.SubWithdrawalAccountsMissing withdrawalsFromUnknownAccounts
            encodeSubmissionError (inLedger inDijkstra failure)
                `shouldBe` unknownCredentialError

        specify "account missing at the start of the batch, in a sub-transaction" $ do
            let failure = Di.SubWithdrawalAccountsMissingFromOriginal withdrawalsFromUnknownAccounts
            encodeSubmissionError (inLedger inDijkstra failure)
                `shouldBe` unknownCredentialError

        specify "wrong network wins over the missing account it causes" $ do
            let wrongNetwork = Di.WithdrawalAddressesWithWrongNetwork Testnet (NESet.singleton mainnetAccountA)
            let missingAccount = Di.WithdrawalAccountsMissing (Withdrawals $ Map.singleton mainnetAccountA (Coin 2))
            let failures = Ledger.injectFailure @"LEDGER" <$> wrongNetwork :| [missingAccount]
            encodeSubmissionError (inDijkstra failures)
                `shouldBe` wrongNetworkError

        specify "partial withdrawal in legacy mode" $ do
            let failure = Di.WithdrawalAmountsInexactInLegacyMode (NEMap.singleton accountA partialWithdrawal)
            encodeSubmissionError (inLedger inDijkstra failure)
                `shouldBe` partialWithdrawalError

        specify "withdrawals above the balance at the start of the batch" $ do
            let excessiveWithdrawal = Mismatch { mismatchSupplied = Coin 12, mismatchExpected = Coin 10 }
            let failure = Di.WithdrawalAmountsExceedingOriginalBalance (NEMap.singleton accountA excessiveWithdrawal)
            encodeSubmissionError (inLedger inDijkstra failure)
                `shouldBe` excessiveWithdrawalError

--
-- Helpers
--

-- | Wrap a single failure of any rule below LEDGER into a submission error.
inLedger
    :: forall era failure. (Ledger.InjectRuleFailure "LEDGER" failure era)
    => (NonEmpty (Ledger.EraRuleFailure "LEDGER" era) -> SubmitTransactionError Block)
    -> failure era
    -> SubmitTransactionError Block
inLedger inEra =
    inEra . pure . Ledger.injectFailure @"LEDGER"

-- | Keep only the error code and data; messages are free text.
encodeSubmissionError :: SubmitTransactionError Block -> Json.Value
encodeSubmissionError =
    inefficientEncodingToValue . encodeSubmitTransactionError encodeCodeAndData
  where
    encodeCodeAndData :: Rpc.FaultCode -> String -> Maybe Json -> Json
    encodeCodeAndData code _message details =
        Encoding.pairs $ "code" .= code <> foldMap (Encoding.pair "data") details

--
-- Fixtures
--

-- Current epoch 100 and a limit of 110: valid retirement epochs are 101 to 110.
retiringTooEarly :: Mismatch r EpochNo
retiringTooEarly =
    Mismatch { mismatchSupplied = EpochNo 100, mismatchExpected = EpochNo 100 }

retiringTooEarlyLimit :: Mismatch r EpochNo
retiringTooEarlyLimit =
    Mismatch { mismatchSupplied = EpochNo 100, mismatchExpected = EpochNo 110 }

retiringTooEarlyError :: Json.Value
retiringTooEarlyError = [aesonQQ|
    { "code": 3142
    , "data": { "currentEpoch": 100, "declaredEpoch": 100, "firstInvalidEpoch": 111 }
    }
|]

retiringTooLate :: Mismatch r EpochNo
retiringTooLate =
    Mismatch { mismatchSupplied = EpochNo 111, mismatchExpected = EpochNo 100 }

retiringTooLateLimit :: Mismatch r EpochNo
retiringTooLateLimit =
    Mismatch { mismatchSupplied = EpochNo 111, mismatchExpected = EpochNo 110 }

retiringTooLateError :: Json.Value
retiringTooLateError = [aesonQQ|
    { "code": 3142
    , "data": { "currentEpoch": 100, "declaredEpoch": 111, "firstInvalidEpoch": 111 }
    }
|]

poolId :: KeyHash StakePool
poolId =
    KeyHash "11111111111111111111111111111111111111111111111111111111"

vrfKeyHash :: VRFVerKeyHash r
vrfKeyHash =
    VRFVerKeyHash "2222222222222222222222222222222222222222222222222222222222222222"

vrfKeyAlreadyRegisteredError :: Json.Value
vrfKeyAlreadyRegisteredError = [aesonQQ|
    { "code": 3176
    , "data":
        { "stakePool": { "id": "pool1zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zdqn7r6" }
        , "vrfVerificationKeyHash": "2222222222222222222222222222222222222222222222222222222222222222"
        }
    }
|]

accountA :: AccountAddress
accountA =
    AccountAddress Testnet (AccountId credentialA)

accountB :: AccountAddress
accountB =
    AccountAddress Testnet (AccountId credentialB)

mainnetAccountA :: AccountAddress
mainnetAccountA =
    AccountAddress Mainnet (AccountId credentialA)

credentialA :: Credential Staking
credentialA =
    KeyHashObj (KeyHash "33333333333333333333333333333333333333333333333333333333")

credentialB :: Credential Staking
credentialB =
    KeyHashObj (KeyHash "44444444444444444444444444444444444444444444444444444444")

-- Account A comes first in the map, whatever the order of the list.
withdrawalsFromUnknownAccounts :: Withdrawals
withdrawalsFromUnknownAccounts =
    Withdrawals $ fromList [ (accountB, Coin 1), (accountA, Coin 2) ]

unknownCredentialError :: Json.Value
unknownCredentialError = [aesonQQ|
    { "code": 3146
    , "data":
        { "unknownCredential": "33333333333333333333333333333333333333333333333333333333"
        , "from": "verificationKey"
        }
    }
|]

wrongNetworkError :: Json.Value
wrongNetworkError = [aesonQQ|
    { "code": 3124
    , "data":
        { "discriminatedType": "rewardAccount"
        , "invalidEntities": [ "stake1uyenxvenxvenxvenxvenxvenxvenxvenxvenxvenxvenxvczes76j" ]
        , "expectedNetwork": "testnet"
        }
    }
|]

partialWithdrawal :: Mismatch r Coin
partialWithdrawal =
    Mismatch { mismatchSupplied = Coin 5, mismatchExpected = Coin 8 }

partialWithdrawalError :: Json.Value
partialWithdrawalError = [aesonQQ|
    { "code": 3141
    , "data":
        { "incompleteWithdrawals":
            { "stake_test1uqenxvenxvenxvenxvenxvenxvenxvenxvenxvenxvenxvc9n6u70": { "ada": { "lovelace": 5 } }
            }
        }
    }
|]

excessiveWithdrawalError :: Json.Value
excessiveWithdrawalError = [aesonQQ|
    { "code": 3174
    , "data":
        { "excessiveWithdrawals":
            [ { "account": "stake_test1uqenxvenxvenxvenxvenxvenxvenxvenxvenxvenxvenxvc9n6u70"
              , "withdrawn": { "ada": { "lovelace": 12 } }
              , "balance": { "ada": { "lovelace": 10 } }
              }
            ]
        }
    }
|]
