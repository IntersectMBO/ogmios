--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger.PredicateFailure.Mary where

import Ogmios.Prelude

import Cardano.Ledger.Address
    ( unWithdrawals
    )
import Cardano.Ledger.BaseTypes
    ( mismatchSupplied
    )
import Ogmios.Data.Ledger.PredicateFailure
    ( MultiEraPredicateFailure (..)
    )
import Ogmios.Data.Ledger.PredicateFailure.Allegra
    ( encodeUtxoFailure
    )
import Ogmios.Data.Ledger.PredicateFailure.Shelley
    ( encodeDelegsFailure
    , encodeUtxowFailure
    )

import qualified Cardano.Ledger.Shelley.Rules as Sh
import qualified Data.Map.NonEmpty as NEMap

encodeLedgerFailure
    :: Sh.ShelleyLedgerPredFailure MaryEra
    -> MultiEraPredicateFailure
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        encodeUtxowFailure (encodeUtxoFailure ShelleyBasedEraMary) e
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
