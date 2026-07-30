module Main (main) where

import Test.Tasty (DependencyType(..), defaultMain, dependentTestGroup, testGroup)
import Test.Integration.Constitution (constitutionTests)
import Test.Integration.DelegateRepresentatives (delegateRepresentativesTests)
import Test.Integration.Env (withTestEnv)
import Test.Integration.Epoch (epochTests)
import Test.Integration.LedgerTip (ledgerTipTests)
import Test.Integration.LiveStakeDistribution (liveStakeDistributionTests)
import Test.Integration.NetworkTip (networkTipTests)
import Test.Integration.ProtocolParameters (protocolParametersTests)
import Test.Integration.RewardAccountSummaries (rewardAccountSummariesTests)
import Test.Integration.StakePools (stakePoolsTests)
import Test.Integration.TxSubmission (txSubmissionTests)
import Test.Integration.Utxo (utxoTests)

-- The query tests compare ogmios against cardano-cli on a ledger nothing is
-- moving underneath them, so submitting transactions has to wait until they
-- have all finished — 'AllFinish' rather than 'AllSucceed', since a failing
-- comparison is no reason to skip submission coverage.
main :: IO ()
main = defaultMain $
  withTestEnv $ \getEnv ->
    dependentTestGroup "Ogmios Integration Tests" AllFinish
      [ testGroup "Queries"
          [ utxoTests getEnv
          , protocolParametersTests getEnv
          , ledgerTipTests getEnv
          , networkTipTests getEnv
          , epochTests getEnv
          , stakePoolsTests getEnv
          , rewardAccountSummariesTests getEnv
          , liveStakeDistributionTests getEnv
          , constitutionTests getEnv
          , delegateRepresentativesTests getEnv
          ]
      , txSubmissionTests getEnv
      ]
