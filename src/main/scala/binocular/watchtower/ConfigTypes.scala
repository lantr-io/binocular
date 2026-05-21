package binocular.watchtower

import scalus.uplc.builtin.*
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.cardano.onchain.plutus.v3.TxOutRef

// Scalus mirror of ft-bifrost-bridge `lib/bifrost/types/config.ak::ConfigDatum`. Field order is
// positional in the Plutus Constr — keep it identical to the .ak record so `config[N]` reads on the
// bridge validators line up. All hash/policy/asset fields are ByteStrings; min_stake is an Int.
//
// The fields read on the peg-in completion path are: 0 bridged_token_policy_id,
// 1 bridged_token_asset_name, 6 completed_peg_ins_merkle_tree_policy_id,
// 7 completed_peg_ins_merkle_tree_asset_name, 10 peg_in_withdraw_script_hash,
// 12 legit_treasury_movement_and_peg_in_spent_verifier_script_hash (+ 11 read by bridged_token but
// needn't match). The rest are dummies for the peg-in-only demo.
case class ConfigDatum(
    bridgedTokenPolicyId: ByteString,
    bridgedTokenAssetName: ByteString,
    sourceChainMerkleTreePolicyId: ByteString,
    sourceChainMerkleTreeAssetName: ByteString,
    blockHeaderMerkleTreePolicyId: ByteString,
    blockHeaderMerkleTreeAssetName: ByteString,
    completedPegInsMerkleTreePolicyId: ByteString,
    completedPegInsMerkleTreeAssetName: ByteString,
    completedPegOutsMerkleTreePolicyId: ByteString,
    completedPegOutsMerkleTreeAssetName: ByteString,
    pegInWithdrawScriptHash: ByteString,
    pegOutWithdrawScriptHash: ByteString,
    legitTmAndPegInSpentVerifierScriptHash: ByteString,
    legitTmAndPegOutProducedVerifierScriptHash: ByteString,
    legitTmAndPegOutNotProducedVerifierScriptHash: ByteString,
    treasuryNftPolicyId: ByteString,
    treasuryNftAssetName: ByteString,
    minStake: BigInt
) derives FromData,
      ToData

// Scalus mirror of `completed-peg-ins-merkle-tree.ak::CompletedPegInsMerkleTreeDatum`. The MPF root
// of completed peg-ins; minted with the empty root (32 zero bytes), then spent+recreated with each
// peg-in inserted on completion.
case class CompletedPegInsMerkleTreeDatum(
    root: ByteString
) derives FromData,
      ToData

// Mint redeemer for `completed-peg-ins-merkle-tree.ak::MintRedeemer { input_ref }` — the one-shot
// UTxO consumed by the mint (fixes the NFT asset name = hash_output_ref(input_ref)).
case class CompletedPegInsMintRedeemer(
    inputRef: TxOutRef
) derives FromData,
      ToData
