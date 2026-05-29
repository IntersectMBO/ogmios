--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger.PredicateFailure.Babbage where

import Ogmios.Prelude

import Cardano.Ledger.Address (
    unWithdrawals,
 )
import Cardano.Ledger.BaseTypes (
    Mismatch (..),
    mismatchSupplied,
 )
import Cardano.Ledger.Core (
    EraRule,
 )
import Control.State.Transition (
    STS (..),
 )
import Ogmios.Data.Ledger.PredicateFailure (
    MultiEraPredicateFailure (..),
    TxOutInAnyEra (..),
 )
import Ogmios.Data.Ledger.PredicateFailure.Shelley (
    encodeDelegsFailure,
 )

import qualified Ogmios.Data.Ledger.PredicateFailure.Alonzo as Alonzo

import qualified Cardano.Ledger.Babbage.Rules as Ba
import qualified Cardano.Ledger.Shelley.Rules as Sh
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Set.NonEmpty as NESet

encodeLedgerFailure ::
    Sh.ShelleyLedgerPredFailure BabbageEra ->
    MultiEraPredicateFailure
encodeLedgerFailure = \case
    Sh.UtxowFailure e ->
        encodeUtxowFailure AlonzoBasedEraBabbage (Alonzo.encodeUtxosFailure AlonzoBasedEraBabbage) e
    Sh.DelegsFailure e ->
        encodeDelegsFailure e
    Sh.ShelleyIncompleteWithdrawals ws ->
        IncompleteWithdrawals
            { withdrawals = mismatchSupplied <$> NEMap.toMap ws
            }
    -- TODO: both ledger failures currently fold into IncompleteWithdrawals;
    -- introduce a dedicated WithdrawalsMissingAccounts variant.
    Sh.ShelleyWithdrawalsMissingAccounts ws ->
        IncompleteWithdrawals
            { withdrawals = unWithdrawals ws
            }

encodeUtxowFailure ::
    forall era.
    ( Era era
    , PredicateFailure (EraRule "UTXO" era) ~ Ba.BabbageUtxoPredFailure era
    ) =>
    AlonzoBasedEra era ->
    (PredicateFailure (EraRule "UTXOS" era) -> MultiEraPredicateFailure) ->
    Ba.BabbageUtxowPredFailure era ->
    MultiEraPredicateFailure
encodeUtxowFailure era encodeUtxosFailure = \case
    Ba.MalformedReferenceScripts scripts ->
        MalformedScripts (NESet.toSet scripts)
    Ba.MalformedScriptWitnesses scripts ->
        MalformedScripts (NESet.toSet scripts)
    Ba.ScriptIntegrityHashMismatch (Mismatch providedIntegrityHash computedIntegrityHash) _ ->
        ScriptIntegrityHashMismatch{providedIntegrityHash, computedIntegrityHash}
    Ba.AlonzoInBabbageUtxowPredFailure e ->
        Alonzo.encodeUtxowFailure era (encodeUtxoFailure era encodeUtxosFailure) e
    Ba.UtxoFailure e ->
        encodeUtxoFailure era encodeUtxosFailure e

encodeUtxoFailure ::
    forall era.
    (Era era) =>
    AlonzoBasedEra era ->
    (PredicateFailure (EraRule "UTXOS" era) -> MultiEraPredicateFailure) ->
    Ba.BabbageUtxoPredFailure era ->
    MultiEraPredicateFailure
encodeUtxoFailure era encodeUtxosFailureInEra = \case
    Ba.AlonzoInBabbageUtxoPredFailure e ->
        Alonzo.encodeUtxoFailure era encodeUtxosFailureInEra e
    Ba.IncorrectTotalCollateralField computedTotalCollateral declaredTotalCollateral ->
        TotalCollateralMismatch{computedTotalCollateral, declaredTotalCollateral}
    Ba.BabbageOutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                ( \(out, minAda) ->
                    ( TxOutInAnyEra (toShelleyBasedEra era, out)
                    , Just minAda
                    )
                )
                    <$> toList outs
         in InsufficientAdaInOutput{insufficientlyFundedOutputs}
    Ba.BabbageNonDisjointRefInputs xs ->
        ConflictingInputsAndReferences xs
