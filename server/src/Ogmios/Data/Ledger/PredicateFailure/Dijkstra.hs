--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger.PredicateFailure.Dijkstra where

import Ogmios.Prelude

import Cardano.Ledger.Address
    ( DirectDeposits (..)
    , unWithdrawals
    )
import Cardano.Ledger.Keys
    ( HasKeyRole (coerceKeyRole)
    )
import Cardano.Ledger.Credential
    ( credScriptHash
    )
import Data.Maybe.Strict
    ( StrictMaybe (..)
    )
import Ogmios.Data.Ledger.PredicateFailure
    ( DiscriminatedEntities (..)
    , MultiEraPredicateFailure (..)
    , ScriptPurposeIndexInAnyEra (..)
    , ScriptPurposeItemInAnyEra (..)
    , TxOutInAnyEra (..)
    , ValueInAnyEra (..)
    , pickPredicateFailure
    )
import Ogmios.Data.Ledger.PredicateFailure.Alonzo
    ( encodeCollectErrors
    )
import Ogmios.Data.Ledger.PredicateFailure.Shelley
    ( encodePoolFailure
    )

import qualified Cardano.Ledger.Api as Ledger
import Cardano.Ledger.BaseTypes
    ( Mismatch (..)
    , mismatchSupplied
    )
import qualified Cardano.Ledger.Conway.Rules as Cn
import qualified Cardano.Ledger.Dijkstra.Rules as Dj
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet

-- | Encode a single DijkstraMempoolPredFailure into the era-agnostic
-- MultiEraPredicateFailure vocabulary.
encodeMempoolFailure
    :: Dj.DijkstraMempoolPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeMempoolFailure = \case
    Dj.LedgerFailure e ->
        encodeLedgerFailure e
    Dj.MempoolFailure mempoolError ->
        UnexpectedMempoolError { mempoolError }
    Dj.AllInputsAreSpent ->
        -- "All inputs are spent" = duplicate transaction / already included.
        -- Conway encodes this via ConwayMempoolFailure; we keep the same mapping.
        UnexpectedMempoolError
            { mempoolError = "All inputs are spent. Transaction has probably already been included"
            }

-- | Encode a DijkstraLedgerPredFailure.
encodeLedgerFailure
    :: Dj.DijkstraLedgerPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeLedgerFailure = \case
    Dj.DijkstraUtxowFailure e ->
        encodeUtxowFailure e
    Dj.DijkstraEntitiesFailure e ->
        encodeEntitiesFailure e
    Dj.DijkstraGovFailure e ->
        encodeGovFailure e
    Dj.DijkstraTreasuryValueMismatch (Mismatch providedWithdrawal computedWithdrawal) ->
        TreasuryWithdrawalMismatch { providedWithdrawal, computedWithdrawal }
    Dj.DijkstraTxRefScriptsSizeTooBig (Mismatch (toInteger -> measuredSize) (toInteger -> maximumSize)) ->
        ReferenceScriptsTooLarge { measuredSize, maximumSize }
    Dj.DijkstraSubLedgersFailure e ->
        encodeSubLedgersFailure e

-------------------------------------------------------------------------------
-- UTXOW
-------------------------------------------------------------------------------

encodeUtxowFailure
    :: Dj.DijkstraUtxowPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeUtxowFailure = \case
    Dj.UtxoFailure e ->
        encodeUtxoFailure e
    Dj.InvalidWitnessesUTXOW wits ->
        InvalidSignatures (toList wits)
    Dj.MissingVKeyWitnessesUTXOW keys ->
        MissingSignatures (NESet.toSet keys)
    Dj.MissingScriptWitnessesUTXOW scripts ->
        MissingScriptWitnesses (NESet.toSet scripts)
    Dj.ScriptWitnessNotValidatingUTXOW scripts ->
        FailingScript (NESet.toSet scripts)
    Dj.MissingTxBodyMetadataHash hash ->
        MissingMetadataHash hash
    Dj.MissingTxMetadata hash ->
        MissingMetadata hash
    Dj.ConflictingMetadataHash (Mismatch providedAuxiliaryDataHash computedAuxiliaryDataHash) ->
        MetadataHashMismatch { providedAuxiliaryDataHash, computedAuxiliaryDataHash }
    Dj.InvalidMetadata ->
        InvalidMetadata
    Dj.ExtraneousScriptWitnessesUTXOW scripts ->
        ExtraneousScriptWitnesses (NESet.toSet scripts)
    Dj.MissingRedeemers redeemers ->
        let missingRedeemers = ScriptPurposeItemInAnyEra . (era,) . fst <$> toList redeemers
         in MissingRedeemers { missingRedeemers }
    Dj.MissingRequiredDatums missingDatums _providedDatums ->
        MissingDatums { missingDatums = NESet.toSet missingDatums }
    Dj.NotAllowedSupplementalDatums extraneousDatums _acceptableDatums ->
        ExtraneousDatums { extraneousDatums = NESet.toSet extraneousDatums }
    Dj.PPViewHashesDontMatch (Mismatch providedIntegrityHash computedIntegrityHash) ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Dj.UnspendableUTxONoDatumHash orphanScriptInputs ->
        OrphanScriptInputs { orphanScriptInputs = NESet.toSet orphanScriptInputs }
    Dj.ExtraRedeemers redeemers ->
        let extraneousRedeemers = ScriptPurposeIndexInAnyEra . (era,) <$> toList redeemers
         in ExtraneousRedeemers { extraneousRedeemers }
    Dj.MalformedScriptWitnesses scripts ->
        MalformedScripts (NESet.toSet scripts)
    Dj.MalformedReferenceScripts scripts ->
        MalformedScripts (NESet.toSet scripts)
    Dj.ScriptIntegrityHashMismatch (Mismatch providedIntegrityHash computedIntegrityHash) _ ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    -- Dijkstra-new: guards required by subtransactions but missing from top-level.
    -- NOTE: No existing vocabulary constructor for guard-specific failures.
    -- We map this to MissingScriptWitnesses as the closest semantic match:
    -- guards are credentials (often script-backed) that subtransactions require.
    -- Only script-backed guards yield a ScriptHash; key-credential guards are
    -- dropped from this set (lossy but compiles).
    Dj.MissingRequiredGuards guards ->
        MissingScriptWitnesses (Set.fromList $ mapMaybe credScriptHash $ Set.toList $ NESet.toSet guards)
    -- Dijkstra-new: guard credentials with incorrect datum presence.
    -- NOTE: No exact vocabulary match. Mapped to MalformedScripts as the
    -- closest approximation: Plutus guards missing datum = malformed.
    Dj.MalformedGuardDatums guards ->
        MalformedScripts (Set.fromList $ mapMaybe credScriptHash $ Set.toList $ NESet.toSet guards)
  where
    era = AlonzoBasedEraDijkstra

-------------------------------------------------------------------------------
-- UTXO
-------------------------------------------------------------------------------

encodeUtxoFailure
    :: Dj.DijkstraUtxoPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeUtxoFailure = \case
    Dj.UtxosFailure e ->
        encodeUtxosFailure e
    Dj.BadInputsUTxO inputs ->
        UnknownUtxoReference (NESet.toSet inputs)
    Dj.OutsideValidityIntervalUTxO validityInterval currentSlot ->
        TransactionOutsideValidityInterval { validityInterval, currentSlot }
    Dj.MaxTxSizeUTxO (Mismatch measuredSize maximumSize) ->
        TransactionTooLarge
            { measuredSize = toInteger measuredSize
            , maximumSize = toInteger maximumSize
            }
    Dj.InputSetEmptyUTxO ->
        EmptyInputSet
    Dj.FeeTooSmallUTxO (Mismatch suppliedFee minimumRequiredFee) ->
        TransactionFeeTooSmall { minimumRequiredFee, suppliedFee }
    Dj.ValueNotConservedUTxO (Mismatch consumed produced) ->
        let valueConsumed = ValueInAnyEra (sbera, consumed) in
        let valueProduced = ValueInAnyEra (sbera, produced) in
        ValueNotConserved { valueConsumed, valueProduced }
    Dj.WrongNetwork expectedNetwork invalidAddrs ->
        let invalidEntities = DiscriminatedAddresses (NESet.toSet invalidAddrs) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Dj.OutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (sbera, out)) <$> toList outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs }
    Dj.OutputTooBigUTxO outs ->
        let culpritOutputs = (\(_, _, out) -> TxOutInAnyEra (sbera, out)) <$> toList outs in
        ValueSizeAboveLimit culpritOutputs
    Dj.InsufficientCollateral providedCollateral minimumRequiredCollateral ->
        InsufficientCollateral { providedCollateral, minimumRequiredCollateral }
    Dj.ScriptsNotPaidUTxO utxo ->
        CollateralInputLockedByScript (Map.keys (NEMap.toMap utxo))
    Dj.ExUnitsTooBigUTxO (Mismatch providedExUnits maximumExUnits) ->
        ExecutionUnitsTooLarge { maximumExUnits, providedExUnits }
    Dj.CollateralContainsNonADA value ->
        let valueInAnyEra = ValueInAnyEra (sbera, value) in
        NonAdaValueAsCollateral valueInAnyEra
    Dj.WrongNetworkInTxBody (Mismatch _providedNetwork expectedNetwork) ->
        let invalidEntities = DiscriminatedTransaction in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Dj.OutsideForecast slot ->
        SlotOutsideForeseeableFuture { slot }
    Dj.TooManyCollateralInputs (Mismatch countedCollateralInputs maximumCollateralInputs) ->
        TooManyCollateralInputs
            { maximumCollateralInputs = fromIntegral maximumCollateralInputs
            , countedCollateralInputs = fromIntegral countedCollateralInputs
            }
    Dj.NoCollateralInputs ->
        MissingCollateralInputs
    Dj.IncorrectTotalCollateralField computedTotalCollateral declaredTotalCollateral ->
        TotalCollateralMismatch { computedTotalCollateral, declaredTotalCollateral }
    Dj.BabbageOutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\(out, minAda) ->
                    ( TxOutInAnyEra (sbera, out)
                    , Just minAda
                    )
                ) <$> toList outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs }
    Dj.BabbageNonDisjointRefInputs xs ->
        ConflictingInputsAndReferences xs
    Dj.PtrPresentInCollateralReturn out ->
        -- NOTE: No dedicated vocabulary constructor. This is a sub-case of
        -- "bad collateral return"; mapped to BootstrapAddressAttributesTooLarge
        -- as the closest semantic match available.
        let culpritOutputs = [TxOutInAnyEra (sbera, out)] in
        BootstrapAddressAttributesTooLarge { culpritOutputs }
    Dj.WithdrawalsExceedAccountBalance ws ->
        -- Dijkstra-new: subtx withdrawals exceed account balance.
        -- Mapped to IncompleteWithdrawals as the closest semantic match.
        IncompleteWithdrawals
            { withdrawals = mismatchSupplied <$> NEMap.toMap ws
            }
  where
    sbera = ShelleyBasedEraDijkstra

-------------------------------------------------------------------------------
-- UTXOS
-------------------------------------------------------------------------------

encodeUtxosFailure
    :: Cn.ConwayUtxosPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeUtxosFailure = \case
    Cn.ValidationTagMismatch validationTag mismatchReason ->
        ValidationTagMismatch { validationTag, mismatchReason }
    Cn.CollectErrors errors ->
        pickPredicateFailure (encodeCollectErrors AlonzoBasedEraDijkstra (toList errors))

-------------------------------------------------------------------------------
-- ENTITIES (replaces Conway's CERTS+Withdrawals at the LEDGER level)
-------------------------------------------------------------------------------

encodeEntitiesFailure
    :: Dj.EntitiesPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeEntitiesFailure = \case
    Dj.CertsFailure e ->
        encodeCertsFailure e
    Dj.MissingAccountsInWithdrawals ws ->
        -- Same mapping as Conway's ConwayWithdrawalsMissingAccounts
        IncompleteWithdrawals
            { withdrawals = unWithdrawals ws
            }
    Dj.IncompleteWithdrawals ws ->
        IncompleteWithdrawals
            { withdrawals = mismatchSupplied <$> NEMap.toMap ws
            }
    Dj.ExceededBalancesInWithdrawals ws ->
        -- Dijkstra non-legacy mode: withdrawals exceed account balance.
        IncompleteWithdrawals
            { withdrawals = mismatchSupplied <$> NEMap.toMap ws
            }
    Dj.MissingAccountsInDirectDeposits (DirectDeposits dd) ->
        -- Dijkstra-new: direct deposits to unregistered accounts.
        -- Mapped to IncompleteWithdrawals as the closest semantic match
        -- (both deal with account-address validity).
        IncompleteWithdrawals
            { withdrawals = dd
            }
    Dj.WrongNetworkInWithdrawals expectedNetwork invalidAccts ->
        let invalidEntities = DiscriminatedRewardAccounts (NESet.toSet invalidAccts) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Dj.WrongNetworkInDirectDeposits expectedNetwork invalidAccts ->
        let invalidEntities = DiscriminatedRewardAccounts (NESet.toSet invalidAccts) in
        NetworkMismatch { expectedNetwork, invalidEntities }

-------------------------------------------------------------------------------
-- GOV
-------------------------------------------------------------------------------

encodeGovFailure
    :: Dj.DijkstraGovPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeGovFailure = \case
    Dj.GovActionsDoNotExist (toList -> fromList -> governanceActions) ->
        UnknownGovernanceActions { governanceActions }
    Dj.MalformedProposal _govAction ->
        InvalidProtocolParametersUpdate
    Dj.ProposalProcedureNetworkIdMismatch rewardAccount expectedNetwork ->
        NetworkMismatch
            { expectedNetwork
            , invalidEntities =
                DiscriminatedRewardAccounts (Set.singleton rewardAccount)
            }
    Dj.TreasuryWithdrawalsNetworkIdMismatch rewardAccounts expectedNetwork ->
        NetworkMismatch
            { expectedNetwork
            , invalidEntities =
                DiscriminatedRewardAccounts (NESet.toSet rewardAccounts)
            }
    Dj.ProposalDepositIncorrect (Mismatch providedDeposit (SJust -> expectedDeposit)) ->
        GovernanceProposalDepositMismatch
            { providedDeposit
            , expectedDeposit
            }
    Dj.DisallowedVoters (toList -> voters) ->
        UnauthorizedVotes voters
    Dj.ConflictingCommitteeUpdate conflictingMembers ->
        ConflictingCommitteeUpdate
            { conflictingMembers = NESet.toSet conflictingMembers
            }
    Dj.ExpirationEpochTooSmall members ->
        InvalidCommitteeUpdate
            { alreadyRetiredMembers = Map.keysSet (NEMap.toMap members)
            }
    Dj.InvalidPrevGovActionId proposal ->
        InvalidPreviousGovernanceAction $
            case Ledger.pProcGovAction proposal of
                Ledger.ParameterChange actionId _ _guardrail ->
                    [ ( Ledger.pProcAnchor proposal
                      , Ledger.PParamUpdatePurpose
                      , Ledger.unGovPurposeId <$> actionId
                      )
                    ]
                Ledger.HardForkInitiation actionId _ ->
                    [ ( Ledger.pProcAnchor proposal
                      , Ledger.HardForkPurpose
                      , Ledger.unGovPurposeId <$> actionId
                      )
                    ]
                Ledger.UpdateCommittee actionId _ _ _ ->
                    [ ( Ledger.pProcAnchor proposal
                      , Ledger.CommitteePurpose
                      , Ledger.unGovPurposeId <$> actionId
                      )
                    ]
                Ledger.NoConfidence actionId ->
                    [ ( Ledger.pProcAnchor proposal
                      , Ledger.CommitteePurpose
                      , Ledger.unGovPurposeId <$> actionId
                      )
                    ]
                Ledger.NewConstitution actionId _ ->
                    [ ( Ledger.pProcAnchor proposal
                      , Ledger.ConstitutionPurpose
                      , Ledger.unGovPurposeId <$> actionId
                      )
                    ]
                Ledger.TreasuryWithdrawals _withdrawals _guardrail ->
                    []
                Ledger.InfoAction ->
                    []
    Dj.VotingOnExpiredGovAction (toList -> voters) ->
        VotingOnExpiredActions voters
    Dj.ProposalCantFollow _ (Mismatch proposedVersion currentVersion) ->
        InvalidHardForkVersionBump { proposedVersion, currentVersion }
    Dj.InvalidGuardrailsScriptHash providedHash expectedHash ->
        ConstitutionGuardrailsHashMismatch { providedHash, expectedHash }
    Dj.DisallowedProposalDuringBootstrap _ ->
        UnauthorizedGovernanceAction
    Dj.DisallowedVotesDuringBootstrap votes ->
        UnauthorizedVotes (toList votes)
    Dj.VotersDoNotExist voters ->
        UnknownVoters (toList voters)
    Dj.ZeroTreasuryWithdrawals _ ->
        EmptyTreasuryWithdrawal
    Dj.ProposalReturnAccountDoesNotExist (Ledger.AccountAddress _ (Ledger.unAccountId -> unknownCredential)) ->
        StakeCredentialNotRegistered { unknownCredential }
    Dj.TreasuryWithdrawalReturnAccountsDoNotExist (NE.head -> Ledger.AccountAddress _ (Ledger.unAccountId -> unknownCredential)) ->
        StakeCredentialNotRegistered { unknownCredential }
    Dj.UnelectedCommitteeVoters voters ->
        UnknownVoters
            { unknownVoters = Ledger.CommitteeVoter <$> toList voters
            }

-------------------------------------------------------------------------------
-- CERTS (reused from Conway)
-------------------------------------------------------------------------------

encodeCertsFailure
    :: Cn.ConwayCertsPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeCertsFailure = \case
    Cn.WithdrawalsNotInRewardsCERTS ws ->
        IncompleteWithdrawals { withdrawals = unWithdrawals ws }
    Cn.CertFailure e ->
        encodeCertFailure e

encodeCertFailure
    :: Cn.ConwayCertPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeCertFailure = \case
    Cn.DelegFailure e ->
        encodeDelegFailure e
    Cn.PoolFailure e ->
        encodePoolFailure e
    Cn.GovCertFailure e ->
        encodeGovCertFailure e

encodeDelegFailure
    :: Cn.ConwayDelegPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeDelegFailure = \case
    Cn.IncorrectDepositDELEG providedDeposit ->
        DepositMismatch { providedDeposit, expectedDeposit = SNothing }
    Cn.StakeKeyRegisteredDELEG knownCredential ->
        StakeCredentialAlreadyRegistered { knownCredential }
    Cn.StakeKeyNotRegisteredDELEG unknownCredential ->
        StakeCredentialNotRegistered { unknownCredential }
    Cn.StakeKeyHasNonZeroAccountBalanceDELEG rewardAccountBalance ->
        RewardAccountNotEmpty { rewardAccountBalance }
    Cn.DelegateeDRepNotRegisteredDELEG (coerceKeyRole -> unknownCredential) ->
        StakeCredentialNotRegistered { unknownCredential }
    Cn.DelegateeStakePoolNotRegisteredDELEG poolId ->
        UnknownStakePool poolId
    Cn.DepositIncorrectDELEG (Mismatch providedDeposit expectedDeposit) ->
        DepositMismatch
            { providedDeposit
            , expectedDeposit = SJust expectedDeposit
            }
    Cn.RefundIncorrectDELEG (Mismatch providedDeposit expectedDeposit) ->
        DepositMismatch
            { providedDeposit
            , expectedDeposit = SJust expectedDeposit
            }

-------------------------------------------------------------------------------
-- GovCert (dijkstra-specific type)
-------------------------------------------------------------------------------

encodeGovCertFailure
    :: Dj.DijkstraGovCertPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeGovCertFailure = \case
    Dj.DijkstraDRepAlreadyRegistered knownDelegateRepresentative ->
        DRepAlreadyRegistered { knownDelegateRepresentative }
    Dj.DijkstraDRepNotRegistered unknownDelegateRepresentative ->
        DRepNotRegistered { unknownDelegateRepresentative }
    Dj.DijkstraDRepIncorrectDeposit (Mismatch providedDeposit (SJust -> expectedDeposit)) ->
        DepositMismatch { providedDeposit, expectedDeposit }
    Dj.DijkstraCommitteeHasPreviouslyResigned unknownConstitutionalCommitteeMember ->
        UnknownConstitutionalCommitteeMember { unknownConstitutionalCommitteeMember }
    Dj.DijkstraDRepIncorrectRefund (Mismatch providedDeposit (SJust -> expectedDeposit)) ->
        DepositMismatch { providedDeposit, expectedDeposit }
    Dj.DijkstraCommitteeIsUnknown unknownConstitutionalCommitteeMember ->
        UnknownConstitutionalCommitteeMember { unknownConstitutionalCommitteeMember }

-------------------------------------------------------------------------------
-- SUBLEDGERS / SUBLEDGER (subtransaction processing)
-------------------------------------------------------------------------------

encodeSubLedgersFailure
    :: Dj.DijkstraSubLedgersPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubLedgersFailure = \case
    Dj.SubLedgerFailure e ->
        encodeSubLedgerFailure e

encodeSubLedgerFailure
    :: Dj.DijkstraSubLedgerPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubLedgerFailure = \case
    Dj.SubUtxowFailure e ->
        encodeSubUtxowFailure e
    Dj.SubEntitiesFailure e ->
        encodeSubEntitiesFailure e
    Dj.SubGovFailure e ->
        encodeSubGovFailure e
    Dj.SubTreasuryValueMismatch (Mismatch providedWithdrawal computedWithdrawal) ->
        TreasuryWithdrawalMismatch { providedWithdrawal, computedWithdrawal }

-------------------------------------------------------------------------------
-- SUBUTXOW
-------------------------------------------------------------------------------

encodeSubUtxowFailure
    :: Dj.DijkstraSubUtxowPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubUtxowFailure = \case
    Dj.SubUtxoFailure e ->
        encodeSubUtxoFailure e
    Dj.SubInvalidWitnessesUTXOW wits ->
        InvalidSignatures (toList wits)
    Dj.SubMissingVKeyWitnessesUTXOW keys ->
        MissingSignatures (NESet.toSet keys)
    Dj.SubScriptWitnessNotValidatingUTXOW scripts ->
        FailingScript (NESet.toSet scripts)
    Dj.SubMissingTxBodyMetadataHash hash ->
        MissingMetadataHash hash
    Dj.SubMissingTxMetadata hash ->
        MissingMetadata hash
    Dj.SubConflictingMetadataHash (Mismatch providedAuxiliaryDataHash computedAuxiliaryDataHash) ->
        MetadataHashMismatch { providedAuxiliaryDataHash, computedAuxiliaryDataHash }
    Dj.SubInvalidMetadata ->
        InvalidMetadata
    Dj.SubMissingRedeemers redeemers ->
        let missingRedeemers = ScriptPurposeItemInAnyEra . (era,) . fst <$> toList redeemers
         in MissingRedeemers { missingRedeemers }
    Dj.SubMissingRequiredDatums missingDatums _providedDatums ->
        MissingDatums { missingDatums = NESet.toSet missingDatums }
    Dj.SubNotAllowedSupplementalDatums extraneousDatums _acceptableDatums ->
        ExtraneousDatums { extraneousDatums = NESet.toSet extraneousDatums }
    Dj.SubPPViewHashesDontMatch (Mismatch providedIntegrityHash computedIntegrityHash) ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Dj.SubUnspendableUTxONoDatumHash orphanScriptInputs ->
        OrphanScriptInputs { orphanScriptInputs = NESet.toSet orphanScriptInputs }
    Dj.SubExtraRedeemers redeemers ->
        let extraneousRedeemers = ScriptPurposeIndexInAnyEra . (era,) <$> toList redeemers
         in ExtraneousRedeemers { extraneousRedeemers }
    Dj.SubMalformedScriptWitnesses scripts ->
        MalformedScripts (NESet.toSet scripts)
    Dj.SubMalformedReferenceScripts scripts ->
        MalformedScripts (NESet.toSet scripts)
    Dj.SubScriptIntegrityHashMismatch (Mismatch providedIntegrityHash computedIntegrityHash) _ ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Dj.SubMalformedGuardDatums guards ->
        MalformedScripts (Set.fromList $ mapMaybe credScriptHash $ Set.toList $ NESet.toSet guards)
  where
    era = AlonzoBasedEraDijkstra

-------------------------------------------------------------------------------
-- SUBUTXO
-------------------------------------------------------------------------------

encodeSubUtxoFailure
    :: Dj.DijkstraSubUtxoPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubUtxoFailure = \case
    Dj.SubBadInputsUTxO inputs ->
        UnknownUtxoReference (NESet.toSet inputs)
    Dj.SubOutsideValidityIntervalUTxO validityInterval currentSlot ->
        TransactionOutsideValidityInterval { validityInterval, currentSlot }
    Dj.SubMaxTxSizeUTxO (Mismatch measuredSize maximumSize) ->
        TransactionTooLarge
            { measuredSize = toInteger measuredSize
            , maximumSize = toInteger maximumSize
            }
    Dj.SubInputSetEmptyUTxO ->
        EmptyInputSet
    Dj.SubWrongNetwork expectedNetwork invalidAddrs ->
        let invalidEntities = DiscriminatedAddresses (NESet.toSet invalidAddrs) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Dj.SubOutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (sbera, out)) <$> toList outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs }
    Dj.SubOutputTooBigUTxO outs ->
        let culpritOutputs = (\(_, _, out) -> TxOutInAnyEra (sbera, out)) <$> toList outs in
        ValueSizeAboveLimit culpritOutputs
    Dj.SubWrongNetworkInTxBody (Mismatch _providedNetwork expectedNetwork) ->
        let invalidEntities = DiscriminatedTransaction in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Dj.SubOutsideForecast slot ->
        SlotOutsideForeseeableFuture { slot }
    Dj.SubBabbageOutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\(out, minAda) ->
                    ( TxOutInAnyEra (sbera, out)
                    , Just minAda
                    )
                ) <$> toList outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs }
  where
    sbera = ShelleyBasedEraDijkstra

-------------------------------------------------------------------------------
-- SUBENTITIES
-------------------------------------------------------------------------------

encodeSubEntitiesFailure
    :: Dj.SubEntitiesPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubEntitiesFailure = \case
    Dj.SubCertsFailure e ->
        encodeSubCertsFailure e
    Dj.SubMissingAccountsInWithdrawals ws ->
        IncompleteWithdrawals
            { withdrawals = unWithdrawals ws
            }
    Dj.SubExceededBalancesInWithdrawals ws ->
        IncompleteWithdrawals
            { withdrawals = mismatchSupplied <$> NEMap.toMap ws
            }
    Dj.SubMissingAccountsInDirectDeposits (DirectDeposits dd) ->
        IncompleteWithdrawals
            { withdrawals = dd
            }
    Dj.SubWrongNetworkInWithdrawals expectedNetwork invalidAccts ->
        let invalidEntities = DiscriminatedRewardAccounts (NESet.toSet invalidAccts) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Dj.SubWrongNetworkInDirectDeposits expectedNetwork invalidAccts ->
        let invalidEntities = DiscriminatedRewardAccounts (NESet.toSet invalidAccts) in
        NetworkMismatch { expectedNetwork, invalidEntities }

-------------------------------------------------------------------------------
-- SUBGOV (newtype over DijkstraGovPredFailure)
-------------------------------------------------------------------------------

encodeSubGovFailure
    :: Dj.DijkstraSubGovPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubGovFailure (Dj.DijkstraSubGovPredFailure e) =
    encodeGovFailure e

-------------------------------------------------------------------------------
-- SUBCERTS / SUBCERT chain
-------------------------------------------------------------------------------

encodeSubCertsFailure
    :: Dj.DijkstraSubCertsPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubCertsFailure (Dj.SubCertFailure e) =
    encodeSubCertFailure e

encodeSubCertFailure
    :: Dj.DijkstraSubCertPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubCertFailure = \case
    Dj.SubDelegFailure e ->
        encodeSubDelegFailure e
    Dj.SubPoolFailure e ->
        encodeSubPoolFailure e
    Dj.SubGovCertFailure e ->
        encodeSubGovCertFailure e

-- | SubDeleg is a newtype around ConwayDelegPredFailure.
encodeSubDelegFailure
    :: Dj.DijkstraSubDelegPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubDelegFailure (Dj.DijkstraSubDelegPredFailure e) =
    encodeDelegFailure e

-- | SubPool is a newtype around ShelleyPoolPredFailure.
encodeSubPoolFailure
    :: Dj.DijkstraSubPoolPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubPoolFailure (Dj.DijkstraSubPoolPredFailure e) =
    encodePoolFailure e

-- | SubGovCert is a newtype around DijkstraGovCertPredFailure.
encodeSubGovCertFailure
    :: Dj.DijkstraSubGovCertPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubGovCertFailure (Dj.DijkstraSubGovCertPredFailure e) =
    encodeGovCertFailure e
