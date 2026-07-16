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
// 7 completed_peg_ins_merkle_tree_asset_name, 10 peg_in_withdraw_script_hash (+ 11 read by
// bridged_token but needn't match). Index 12 — formerly the (retired) legit_TM_verifier — is now
// the peg-in CLOSE verifier hash: peg_in.ak's Cancel branch delegates the F4/F5 close checks to a
// withdrawal from this script. It's a runtime config field, so the close verifier can be deployed +
// wired via a config update with no peg_in recompile / PIR re-mint. Dummy (Cancel disabled) until
// the F1–F6 failure-mode milestone ships. Index 18 is the config Update/Retire authority
// (Aiken `Option<AuthorizationMethod>`); index 19 the fBTC mint-checker withdraw script hash.
// The rest are dummies for the peg-in-only demo.
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
    pegInCloseVerifierScriptHash: ByteString,
    legitTmAndPegOutProducedVerifierScriptHash: ByteString,
    legitTmAndPegOutNotProducedVerifierScriptHash: ByteString,
    treasuryNftPolicyId: ByteString,
    treasuryNftAssetName: ByteString,
    minStake: BigInt,
    // Index 18: the authority allowed to Update/Retire the config UTxO
    // (config.ak spend handler). None = permanently frozen.
    updateAuth: scalus.cardano.onchain.plutus.prelude.Option[AuthorizationMethod],
    // Index 19: the withdraw script carrying ALL fBTC mint/burn rules; the
    // immutable bridged_token policy only requires it to run in the tx.
    bridgedTokenMintCheckerScriptHash: ByteString
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

// Spend redeemer for `completed-peg-ins-merkle-tree.ak::SpendRedeemer`. The spend handler reads
// config[10] = peg_in_withdraw_script_hash, then requires the peg_in withdraw redeemer at
// `pegInWithdrawRedeemerIndex` to be a `Withdraw(Script(peg_in_withdraw_hash))` carrying a
// CompletePegIn action, and that a withdrawal from that script is present. Both indices are
// computed from the assembled tx (see PegInCompleteTx).
case class CompletedPegInsSpendRedeemer(
    configRefInputIndex: BigInt,
    pegInWithdrawRedeemerIndex: BigInt
) derives FromData,
      ToData

// Mint redeemer for `bridged-token.ak::MintRedeemer` (the fBTC policy). The policy is a pure
// delegator: it reads config[19] = bridged_token_mint_checker_script_hash from the config ref
// input at `configRefInputIndex` and requires that withdraw script to run in the tx. All actual
// mint/burn rules live in the checker (FbtcMintCheckerRedeemer below).
case class BridgedTokenMintRedeemer(
    configRefInputIndex: BigInt
) derives FromData,
      ToData

// Withdraw redeemer for `fbtc-mint-checker.ak::CheckerRedeemer`. V1 checker rules: exactly one
// asset entry under the fBTC policy with the configured name; mint (>0) requires the peg_in
// withdraw redeemer at `pegWithdrawRedeemerIndex` to be a CompletePegIn, burn (<0) a peg_out
// CompletePegOut. Both indices are computed from the assembled tx.
case class FbtcMintCheckerRedeemer(
    configRefInputIndex: BigInt,
    pegWithdrawRedeemerIndex: BigInt
) derives FromData,
      ToData
