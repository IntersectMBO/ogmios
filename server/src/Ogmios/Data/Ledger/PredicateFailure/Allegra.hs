--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger.PredicateFailure.Allegra where

import Ogmios.Prelude

import Ogmios.Data.Ledger.PredicateFailure
    ( DiscriminatedEntities (..)
    , MultiEraPredicateFailure (..)
    , TxOutInAnyEra (..)
    , ValueInAnyEra (..)
    )
import Ogmios.Data.Ledger.PredicateFailure.Shelley
    ( encodeDelegsFailure
    , encodeUtxowFailure
    )

import Cardano.Ledger.Address
    ( unWithdrawals
    )
import qualified Cardano.Ledger.Allegra.Rules as Al
import Cardano.Ledger.BaseTypes
    ( Mismatch (..)
    , mismatchSupplied
    )
import qualified Cardano.Ledger.Shelley.Rules as Sh
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Set.NonEmpty as NESet

encodeLedgerFailure
    :: Sh.ShelleyLedgerPredFailure AllegraEra
    -> MultiEraPredicateFailure
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        encodeUtxowFailure (encodeUtxoFailure ShelleyBasedEraAllegra) e
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

encodeUtxoFailure
    :: forall era.
        ( Era era
        )
    => ShelleyBasedEra era
    -> Al.AllegraUtxoPredFailure era
    -> MultiEraPredicateFailure
encodeUtxoFailure era = \case
    Al.BadInputsUTxO inputs ->
        UnknownUtxoReference (NESet.toSet inputs)
    Al.OutsideValidityIntervalUTxO validityInterval currentSlot ->
        TransactionOutsideValidityInterval { validityInterval, currentSlot }
    Al.OutputTooBigUTxO outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (era, out)) <$> toList outs in
        ValueSizeAboveLimit culpritOutputs
    Al.MaxTxSizeUTxO (Mismatch measuredSize maximumSize) ->
        TransactionTooLarge
            { measuredSize = toInteger measuredSize
            , maximumSize = toInteger maximumSize
            }
    Al.InputSetEmptyUTxO ->
        EmptyInputSet
    Al.FeeTooSmallUTxO (Mismatch suppliedFee minimumRequiredFee) ->
        TransactionFeeTooSmall { minimumRequiredFee, suppliedFee }
    Al.ValueNotConservedUTxO (Mismatch consumed produced) ->
        let valueConsumed = ValueInAnyEra (era, consumed) in
        let valueProduced = ValueInAnyEra (era, produced) in
        ValueNotConserved { valueConsumed, valueProduced }
    Al.WrongNetwork expectedNetwork invalidAddrs ->
        let invalidEntities = DiscriminatedAddresses (NESet.toSet invalidAddrs) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Al.WrongNetworkWithdrawal expectedNetwork invalidAccts ->
        let invalidEntities = DiscriminatedRewardAccounts (NESet.toSet invalidAccts) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Al.OutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\out -> (TxOutInAnyEra (era, out), Nothing)) <$> toList outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs }
    Al.OutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (era, out)) <$> toList outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs }
    Al.UpdateFailure{} ->
        InvalidProtocolParametersUpdate
