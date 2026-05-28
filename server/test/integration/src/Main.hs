module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Integration.Constitution (constitutionTests)
import Test.Integration.Env (withTestEnv)
import Test.Integration.Epoch (epochTests)
import Test.Integration.LedgerTip (ledgerTipTests)
import Test.Integration.LiveStakeDistribution (liveStakeDistributionTests)
import Test.Integration.NetworkTip (networkTipTests)
import Test.Integration.ProtocolParameters (protocolParametersTests)
import Test.Integration.RewardAccountSummaries (rewardAccountSummariesTests)
import Test.Integration.StakePools (stakePoolsTests)
import Test.Integration.Utxo (utxoTests)

main :: IO ()
main = defaultMain $
  withTestEnv $ \getEnv ->
    testGroup "Ogmios Integration Tests"
      [ utxoTests getEnv
      , protocolParametersTests getEnv
      , ledgerTipTests getEnv
      , networkTipTests getEnv
      , epochTests getEnv
      , stakePoolsTests getEnv
      , rewardAccountSummariesTests getEnv
      , liveStakeDistributionTests getEnv
      , constitutionTests getEnv
      ]
