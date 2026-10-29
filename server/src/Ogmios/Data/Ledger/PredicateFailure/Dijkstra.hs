--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger.PredicateFailure.Dijkstra where

import Ogmios.Prelude

import Cardano.Ledger.Address
    ( AccountAddress
    , DirectDeposits (..)
    , unWithdrawals
    )
import Cardano.Ledger.Alonzo.Scripts
    ( OrdExUnits (..)
    )
import Cardano.Ledger.BaseTypes
    ( Mismatch (..)
    , mismatchSupplied
    )
import Data.Maybe.Strict
    ( StrictMaybe (..)
    )
import Ogmios.Data.Ledger.PredicateFailure
    ( BalanceSnapshot (..)
    , DiscriminatedEntities (..)
    , MultiEraPredicateFailure (..)
    , Network
    , ScriptPurposeIndexInAnyEra (..)
    , ScriptPurposeItemInAnyEra (..)
    , TxOutInAnyEra (..)
    , ValueInAnyEra (..)
    )
import Ogmios.Data.Ledger.PredicateFailure.Conway
    ( encodeCertsFailure
    , encodeDelegFailure
    , encodeInvalidPrevGovActionId
    , encodeUtxosFailure
    )

import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Dijkstra.Rules as Di
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet

encodeMempoolFailure
    :: Di.DijkstraMempoolPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeMempoolFailure = \case
    Di.LedgerFailure e ->
        encodeLedgerFailure e
    Di.MempoolFailure mempoolError ->
        UnexpectedMempoolError { mempoolError }
    Di.AllInputsAreSpent ->
        AllInputsAreSpent

encodeLedgerFailure
    :: Di.DijkstraLedgerPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeLedgerFailure = \case
    Di.DijkstraUtxowFailure e ->
        encodeUtxowFailure e
    Di.DijkstraEntitiesFailure e ->
        encodeEntitiesFailure e
    Di.DijkstraGovFailure e ->
        encodeGovFailure e
    Di.DijkstraTreasuryValueMismatch (Mismatch providedWithdrawal computedWithdrawal) ->
        TreasuryWithdrawalMismatch { providedWithdrawal, computedWithdrawal }
    Di.DijkstraTxRefScriptsSizeTooBig (Mismatch (toInteger -> measuredSize) (toInteger -> maximumSize)) ->
        ReferenceScriptsTooLarge { measuredSize, maximumSize }
    Di.DijkstraSubLedgersFailure e ->
        encodeSubLedgersFailure e

encodeUtxowFailure
    :: Di.DijkstraUtxowPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeUtxowFailure = \case
    Di.UtxoFailure e ->
        encodeUtxoFailure e
    Di.InvalidWitnessesUTXOW wits ->
        InvalidSignatures (toList wits)
    Di.MissingVKeyWitnessesUTXOW keys ->
        MissingSignatures (NESet.toSet keys)
    Di.MissingScriptWitnessesUTXOW scripts ->
        MissingScriptWitnesses (NESet.toSet scripts)
    Di.ScriptWitnessNotValidatingUTXOW scripts ->
        FailingScript (NESet.toSet scripts)
    Di.MissingTxBodyMetadataHash hash ->
        MissingMetadataHash hash
    Di.MissingTxMetadata hash ->
        MissingMetadata hash
    Di.ConflictingMetadataHash (Mismatch providedAuxiliaryDataHash computedAuxiliaryDataHash) ->
        MetadataHashMismatch { providedAuxiliaryDataHash, computedAuxiliaryDataHash }
    Di.InvalidMetadata ->
        InvalidMetadata
    Di.ExtraneousScriptWitnessesUTXOW scripts ->
        ExtraneousScriptWitnesses (NESet.toSet scripts)
    Di.MissingRedeemers redeemers ->
        let missingRedeemers = ScriptPurposeItemInAnyEra . (era,) . fst <$> toList redeemers
         in MissingRedeemers { missingRedeemers }
    Di.MissingRequiredDatums missingDatums _providedDatums ->
        MissingDatums { missingDatums = NESet.toSet missingDatums }
    Di.NotAllowedSupplementalDatums extraneousDatums _acceptableDatums ->
        ExtraneousDatums { extraneousDatums = NESet.toSet extraneousDatums }
    Di.PPViewHashesDontMatch (Mismatch providedIntegrityHash computedIntegrityHash) ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Di.UnspendableUTxONoDatumHash orphanScriptInputs ->
        OrphanScriptInputs { orphanScriptInputs = NESet.toSet orphanScriptInputs }
    Di.ExtraRedeemers redeemers ->
        let extraneousRedeemers = ScriptPurposeIndexInAnyEra . (era,) <$> toList redeemers
         in ExtraneousRedeemers { extraneousRedeemers }
    Di.MalformedScriptWitnesses scripts ->
        MalformedScripts (NESet.toSet scripts)
    Di.MalformedReferenceScripts scripts ->
        MalformedScripts (NESet.toSet scripts)
    Di.ScriptIntegrityHashMismatch (Mismatch providedIntegrityHash computedIntegrityHash) _ ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Di.MissingRequiredGuards guards ->
        MissingRequiredGuards { missingGuards = NESet.toSet guards }
    Di.MalformedGuardDatums guards ->
        MalformedGuardDatums { malformedGuards = NESet.toSet guards }
  where
    era = AlonzoBasedEraDijkstra

encodeUtxoFailure
    :: Di.DijkstraUtxoPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeUtxoFailure = \case
    Di.UtxosFailure e ->
        encodeUtxosFailure AlonzoBasedEraDijkstra e
    Di.BadInputsUTxO inputs ->
        UnknownUtxoReference (NESet.toSet inputs)
    Di.OutsideValidityIntervalUTxO validityInterval currentSlot ->
        TransactionOutsideValidityInterval { validityInterval, currentSlot }
    Di.MaxTxSizeUTxO (Mismatch measuredSize maximumSize) ->
        TransactionTooLarge
            { measuredSize = toInteger measuredSize
            , maximumSize = toInteger maximumSize
            }
    Di.InputSetEmptyUTxO ->
        EmptyInputSet
    Di.FeeTooSmallUTxO (Mismatch suppliedFee minimumRequiredFee) ->
        TransactionFeeTooSmall { minimumRequiredFee, suppliedFee }
    Di.ValueNotConservedUTxO (Mismatch consumed produced) ->
        let valueConsumed = ValueInAnyEra (era, consumed) in
        let valueProduced = ValueInAnyEra (era, produced) in
        ValueNotConserved { valueConsumed, valueProduced }
    Di.WrongNetwork expectedNetwork invalidAddrs ->
        let invalidEntities = DiscriminatedAddresses (NESet.toSet invalidAddrs) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Di.OutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (era, out)) <$> toList outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs }
    Di.OutputTooBigUTxO outs ->
        let culpritOutputs = (\(_, _, out) -> TxOutInAnyEra (era, out)) <$> toList outs in
        ValueSizeAboveLimit culpritOutputs
    Di.InsufficientCollateral providedCollateral minimumRequiredCollateral ->
        InsufficientCollateral { providedCollateral, minimumRequiredCollateral }
    Di.ScriptsNotPaidUTxO utxo ->
        CollateralInputLockedByScript (Map.keys (NEMap.toMap utxo))
    Di.ExUnitsTooBigUTxO (Mismatch (OrdExUnits providedExUnits) (OrdExUnits maximumExUnits)) ->
        ExecutionUnitsTooLarge { maximumExUnits, providedExUnits }
    Di.CollateralContainsNonADA value ->
        let valueInAnyEra = ValueInAnyEra (era, value) in
        NonAdaValueAsCollateral valueInAnyEra
    Di.WrongNetworkInTxBody (Mismatch _providedNetwork expectedNetwork) ->
        let invalidEntities = DiscriminatedTransaction in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Di.OutsideForecast slot ->
        SlotOutsideForeseeableFuture { slot }
    Di.TooManyCollateralInputs (Mismatch countedCollateralInputs maximumCollateralInputs) ->
        TooManyCollateralInputs
            { maximumCollateralInputs = fromIntegral maximumCollateralInputs
            , countedCollateralInputs = fromIntegral countedCollateralInputs
            }
    Di.NoCollateralInputs ->
        MissingCollateralInputs
    Di.IncorrectTotalCollateralField computedTotalCollateral declaredTotalCollateral ->
        TotalCollateralMismatch { computedTotalCollateral, declaredTotalCollateral }
    Di.BabbageOutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\(out, minAda) -> (TxOutInAnyEra (era, out), Just minAda)) <$> toList outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs }
    Di.BabbageNonDisjointRefInputs xs ->
        ConflictingInputsAndReferences xs
    Di.PtrPresentInCollateralReturn out ->
        PtrPresentInCollateralReturn { collateralReturn = TxOutInAnyEra (era, out) }
    -- NOTE: A legacy-mode top-level transaction must balance on its own; this
    -- is reported as a plain value conservation failure.
    Di.ValueNotConservedInLegacyMode (Mismatch consumed produced) ->
        let valueConsumed = ValueInAnyEra (era, consumed) in
        let valueProduced = ValueInAnyEra (era, produced) in
        ValueNotConserved { valueConsumed, valueProduced }
  where
    era = ShelleyBasedEraDijkstra

encodeEntitiesFailure
    :: Di.EntitiesPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeEntitiesFailure = \case
    Di.CertsFailure e ->
        encodeCertsFailure encodePoolFailure encodeGovCertFailure e
    Di.WithdrawalAddressesWithWrongNetwork expectedNetwork accounts ->
        encodeWrongNetworkAccounts expectedNetwork accounts
    -- TODO: missing accounts fold into IncompleteWithdrawals, as in earlier eras.
    Di.WithdrawalAccountsMissing ws ->
        IncompleteWithdrawals { withdrawals = unWithdrawals ws }
    Di.WithdrawalAccountsMissingFromOriginal ws ->
        IncompleteWithdrawals { withdrawals = unWithdrawals ws }
    Di.WithdrawalAmountsInexactInLegacyMode ws ->
        IncompleteWithdrawals { withdrawals = mismatchSupplied <$> NEMap.toMap ws }
    Di.WithdrawalAmountsExceedingOriginalBalance ws ->
        WithdrawalsExceedingOriginalBalance
            { excessiveWithdrawals =
                (\(Mismatch withdrawn balance) -> (withdrawn, balance)) <$> NEMap.toMap ws
            }
    Di.DirectDepositAddressesWithWrongNetwork expectedNetwork accounts ->
        encodeWrongNetworkAccounts expectedNetwork accounts
    Di.DirectDepositAccountsMissing deposits ->
        encodeUnregisteredAccount "DirectDepositAccountsMissing" (unDirectDeposits deposits)
    Di.WrongNetworkInAccountBalanceIntervals expectedNetwork accounts ->
        encodeWrongNetworkAccounts expectedNetwork accounts
    Di.MissingAccountsInAccountBalanceIntervals intervals ->
        encodeUnregisteredAccount "MissingAccountsInAccountBalanceIntervals" (NEMap.toMap intervals)
    Di.BalancesOutsideAccountBalanceIntervals balances ->
        BalancesOutsideAccountBalanceIntervals
            { checkedAgainst = TransactionBalances
            , balancesOutsideIntervals = NEMap.toMap balances
            }
    Di.WrongNetworkInStartingAccountBalanceIntervals expectedNetwork accounts ->
        encodeWrongNetworkAccounts expectedNetwork accounts
    Di.MissingAccountsInStartingAccountBalanceIntervals intervals ->
        encodeUnregisteredAccount "MissingAccountsInStartingAccountBalanceIntervals" (NEMap.toMap intervals)
    Di.BalancesOutsideStartingAccountBalanceIntervals balances ->
        BalancesOutsideAccountBalanceIntervals
            { checkedAgainst = BatchStartBalances
            , balancesOutsideIntervals = NEMap.toMap balances
            }

encodeGovFailure
    :: Di.DijkstraGovPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeGovFailure = \case
    Di.GovActionsDoNotExist (toList -> fromList -> governanceActions) ->
        UnknownGovernanceActions { governanceActions }
    Di.MalformedProposal _govAction ->
        InvalidProtocolParametersUpdate
    Di.ProposalProcedureNetworkIdMismatch rewardAccount expectedNetwork ->
        NetworkMismatch
            { expectedNetwork
            , invalidEntities =
                DiscriminatedRewardAccounts (Set.singleton rewardAccount)
            }
    Di.TreasuryWithdrawalsNetworkIdMismatch rewardAccounts expectedNetwork ->
        NetworkMismatch
            { expectedNetwork
            , invalidEntities =
                DiscriminatedRewardAccounts (NESet.toSet rewardAccounts)
            }
    Di.ProposalDepositIncorrect (Mismatch providedDeposit (SJust -> expectedDeposit)) ->
        GovernanceProposalDepositMismatch
            { providedDeposit
            , expectedDeposit
            }
    Di.DisallowedVoters (toList -> voters) ->
        UnauthorizedVotes voters
    Di.ConflictingCommitteeUpdate conflictingMembers ->
        ConflictingCommitteeUpdate
            { conflictingMembers = NESet.toSet conflictingMembers
            }
    Di.ExpirationEpochTooSmall members ->
        InvalidCommitteeUpdate
            { alreadyRetiredMembers = Map.keysSet (NEMap.toMap members)
            }
    Di.InvalidPrevGovActionId proposal ->
        encodeInvalidPrevGovActionId proposal
    Di.VotingOnExpiredGovAction (toList -> voters) ->
        VotingOnExpiredActions voters
    Di.ProposalCantFollow _ (Mismatch proposedVersion currentVersion) ->
        InvalidHardForkVersionBump { proposedVersion, currentVersion }
    Di.InvalidGuardrailsScriptHash providedHash expectedHash ->
        ConstitutionGuardrailsHashMismatch { providedHash, expectedHash }
    Di.DisallowedProposalDuringBootstrap _ ->
        UnauthorizedGovernanceAction
    Di.DisallowedVotesDuringBootstrap votes ->
        UnauthorizedVotes (toList votes)
    Di.VotersDoNotExist voters ->
        UnknownVoters (toList voters)
    Di.ZeroTreasuryWithdrawals _ ->
        EmptyTreasuryWithdrawal
    Di.ProposalReturnAccountDoesNotExist (Ledger.AccountAddress _ (Ledger.unAccountId -> unknownCredential)) ->
        StakeCredentialNotRegistered { unknownCredential }
    Di.TreasuryWithdrawalReturnAccountsDoNotExist (NE.head -> Ledger.AccountAddress _ (Ledger.unAccountId -> unknownCredential)) ->
        StakeCredentialNotRegistered { unknownCredential }
    Di.UnelectedCommitteeVoters voters ->
        UnknownVoters
            { unknownVoters = Ledger.CommitteeVoter <$> toList voters
            }

encodeGovCertFailure
    :: Di.DijkstraGovCertPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeGovCertFailure = \case
    Di.DijkstraDRepAlreadyRegistered knownDelegateRepresentative ->
        DRepAlreadyRegistered { knownDelegateRepresentative }
    Di.DijkstraDRepNotRegistered unknownDelegateRepresentative ->
        DRepNotRegistered { unknownDelegateRepresentative }
    Di.DijkstraDRepIncorrectDeposit (Mismatch providedDeposit (SJust -> expectedDeposit)) ->
        DepositMismatch { providedDeposit, expectedDeposit }
    Di.DijkstraCommitteeHasPreviouslyResigned unknownConstitutionalCommitteeMember ->
        UnknownConstitutionalCommitteeMember { unknownConstitutionalCommitteeMember }
    Di.DijkstraDRepIncorrectRefund (Mismatch providedDeposit (SJust -> expectedDeposit)) ->
        DepositMismatch { providedDeposit, expectedDeposit }
    Di.DijkstraCommitteeIsUnknown unknownConstitutionalCommitteeMember ->
        UnknownConstitutionalCommitteeMember { unknownConstitutionalCommitteeMember }

encodePoolFailure
    :: Di.DijkstraPoolPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodePoolFailure = \case
    Di.StakePoolNotRegisteredOnKeyPOOL poolId ->
        UnknownStakePool { poolId }
    Di.StakePoolRetirementWrongEpochPOOL (Mismatch currentEpoch listedEpoch) (Mismatch _ firstInvalidEpoch) ->
        InvalidStakePoolRetirementEpoch { currentEpoch, listedEpoch, firstInvalidEpoch }
    Di.StakePoolCostTooLowPOOL (Mismatch declaredCost minimumPoolCost) ->
        StakePoolCostTooLow { declaredCost, minimumPoolCost }
    Di.WrongNetworkPOOL (Mismatch _ expectedNetwork) poolId ->
        let invalidEntities = DiscriminatedPoolRegistrationCertificate poolId in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Di.PoolMedataHashTooBig poolId computedMetadataHashSize ->
        StakePoolMetadataHashTooLarge { poolId, computedMetadataHashSize }
    -- TODO: VRF-key reuse has no dedicated failure; fold into UnknownStakePool,
    -- as the Shelley encoder does.
    Di.VRFKeyHashAlreadyRegistered poolId _vrfKeyHash ->
        UnknownStakePool { poolId }
    Di.BlsKeyInvalidProofOfPossession poolId blsKey ->
        BlsKeyInvalidProofOfPossession { poolId, blsKey }

encodeSubLedgersFailure
    :: Di.DijkstraSubLedgersPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubLedgersFailure = \case
    Di.SubLedgerFailure e ->
        encodeSubLedgerFailure e

-- TODO: sub-transaction failures map like their top-level counterparts; the
-- failing sub-transaction is not reported.
encodeSubLedgerFailure
    :: Di.DijkstraSubLedgerPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubLedgerFailure = \case
    Di.SubUtxowFailure e ->
        encodeSubUtxowFailure e
    Di.SubEntitiesFailure e ->
        encodeSubEntitiesFailure e
    Di.SubGovFailure (Di.DijkstraSubGovPredFailure e) ->
        encodeGovFailure e
    Di.SubTreasuryValueMismatch (Mismatch providedWithdrawal computedWithdrawal) ->
        TreasuryWithdrawalMismatch { providedWithdrawal, computedWithdrawal }

encodeSubUtxowFailure
    :: Di.DijkstraSubUtxowPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubUtxowFailure = \case
    Di.SubUtxoFailure e ->
        encodeSubUtxoFailure e
    Di.SubInvalidWitnessesUTXOW wits ->
        InvalidSignatures (toList wits)
    Di.SubMissingVKeyWitnessesUTXOW keys ->
        MissingSignatures (NESet.toSet keys)
    Di.SubScriptWitnessNotValidatingUTXOW scripts ->
        FailingScript (NESet.toSet scripts)
    Di.SubMissingTxBodyMetadataHash hash ->
        MissingMetadataHash hash
    Di.SubMissingTxMetadata hash ->
        MissingMetadata hash
    Di.SubConflictingMetadataHash (Mismatch providedAuxiliaryDataHash computedAuxiliaryDataHash) ->
        MetadataHashMismatch { providedAuxiliaryDataHash, computedAuxiliaryDataHash }
    Di.SubInvalidMetadata ->
        InvalidMetadata
    Di.SubMissingRedeemers redeemers ->
        let missingRedeemers = ScriptPurposeItemInAnyEra . (era,) . fst <$> toList redeemers
         in MissingRedeemers { missingRedeemers }
    Di.SubMissingRequiredDatums missingDatums _providedDatums ->
        MissingDatums { missingDatums = NESet.toSet missingDatums }
    Di.SubNotAllowedSupplementalDatums extraneousDatums _acceptableDatums ->
        ExtraneousDatums { extraneousDatums = NESet.toSet extraneousDatums }
    Di.SubPPViewHashesDontMatch (Mismatch providedIntegrityHash computedIntegrityHash) ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Di.SubUnspendableUTxONoDatumHash orphanScriptInputs ->
        OrphanScriptInputs { orphanScriptInputs = NESet.toSet orphanScriptInputs }
    Di.SubExtraRedeemers redeemers ->
        let extraneousRedeemers = ScriptPurposeIndexInAnyEra . (era,) <$> toList redeemers
         in ExtraneousRedeemers { extraneousRedeemers }
    Di.SubMalformedScriptWitnesses scripts ->
        MalformedScripts (NESet.toSet scripts)
    Di.SubMalformedReferenceScripts scripts ->
        MalformedScripts (NESet.toSet scripts)
    Di.SubScriptIntegrityHashMismatch (Mismatch providedIntegrityHash computedIntegrityHash) _ ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Di.SubMalformedGuardDatums guards ->
        MalformedGuardDatums { malformedGuards = NESet.toSet guards }
  where
    era = AlonzoBasedEraDijkstra

encodeSubUtxoFailure
    :: Di.DijkstraSubUtxoPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubUtxoFailure = \case
    Di.SubBadInputsUTxO inputs ->
        UnknownUtxoReference (NESet.toSet inputs)
    Di.SubOutsideValidityIntervalUTxO validityInterval currentSlot ->
        TransactionOutsideValidityInterval { validityInterval, currentSlot }
    Di.SubMaxTxSizeUTxO (Mismatch measuredSize maximumSize) ->
        TransactionTooLarge
            { measuredSize = toInteger measuredSize
            , maximumSize = toInteger maximumSize
            }
    Di.SubInputSetEmptyUTxO ->
        EmptyInputSet
    Di.SubWrongNetwork expectedNetwork invalidAddrs ->
        let invalidEntities = DiscriminatedAddresses (NESet.toSet invalidAddrs) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Di.SubOutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (era, out)) <$> toList outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs }
    Di.SubOutputTooBigUTxO outs ->
        let culpritOutputs = (\(_, _, out) -> TxOutInAnyEra (era, out)) <$> toList outs in
        ValueSizeAboveLimit culpritOutputs
    Di.SubWrongNetworkInTxBody (Mismatch _providedNetwork expectedNetwork) ->
        let invalidEntities = DiscriminatedTransaction in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Di.SubOutsideForecast slot ->
        SlotOutsideForeseeableFuture { slot }
    Di.SubBabbageOutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\(out, minAda) -> (TxOutInAnyEra (era, out), Just minAda)) <$> toList outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs }
  where
    era = ShelleyBasedEraDijkstra

encodeSubEntitiesFailure
    :: Di.SubEntitiesPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubEntitiesFailure = \case
    Di.SubCertsFailure e ->
        encodeSubCertsFailure e
    Di.SubWithdrawalAddressesWithWrongNetwork expectedNetwork accounts ->
        encodeWrongNetworkAccounts expectedNetwork accounts
    Di.SubWithdrawalAccountsMissing ws ->
        IncompleteWithdrawals { withdrawals = unWithdrawals ws }
    Di.SubWithdrawalAccountsMissingFromOriginal ws ->
        IncompleteWithdrawals { withdrawals = unWithdrawals ws }
    Di.SubDirectDepositAddressesWithWrongNetwork expectedNetwork accounts ->
        encodeWrongNetworkAccounts expectedNetwork accounts
    Di.SubDirectDepositAccountsMissing deposits ->
        encodeUnregisteredAccount "SubDirectDepositAccountsMissing" (unDirectDeposits deposits)
    Di.SubBalancesOutsideAccountBalanceIntervals balances ->
        BalancesOutsideAccountBalanceIntervals
            { checkedAgainst = TransactionBalances
            , balancesOutsideIntervals = NEMap.toMap balances
            }
    Di.SubMissingAccountsInAccountBalanceIntervals intervals ->
        encodeUnregisteredAccount "SubMissingAccountsInAccountBalanceIntervals" (NEMap.toMap intervals)
    Di.SubWrongNetworkInAccountBalanceIntervals expectedNetwork accounts ->
        encodeWrongNetworkAccounts expectedNetwork accounts

encodeSubCertsFailure
    :: Di.DijkstraSubCertsPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubCertsFailure = \case
    Di.SubCertFailure e ->
        encodeSubCertFailure e

encodeSubCertFailure
    :: Di.DijkstraSubCertPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubCertFailure = \case
    Di.SubDelegFailure (Di.DijkstraSubDelegPredFailure e) ->
        encodeDelegFailure e
    Di.SubPoolFailure (Di.DijkstraSubPoolPredFailure e) ->
        encodePoolFailure e
    Di.SubGovCertFailure (Di.DijkstraSubGovCertPredFailure e) ->
        encodeGovCertFailure e

encodeWrongNetworkAccounts
    :: Network
    -> NESet.NonEmptySet AccountAddress
    -> MultiEraPredicateFailure
encodeWrongNetworkAccounts expectedNetwork accounts =
    let invalidEntities = DiscriminatedRewardAccounts (NESet.toSet accounts) in
    NetworkMismatch { expectedNetwork, invalidEntities }

-- | Report the first unregistered account only. The ledger never reports an
-- empty map; the fallback names the ledger failure to keep the mapping total.
encodeUnregisteredAccount
    :: Text
    -> Map AccountAddress a
    -> MultiEraPredicateFailure
encodeUnregisteredAccount failureName =
    maybe
        (UnexpectedMempoolError { mempoolError = failureName })
        (\(Ledger.AccountAddress _ (Ledger.unAccountId -> unknownCredential), _) ->
            StakeCredentialNotRegistered { unknownCredential }
        )
    . Map.lookupMin
