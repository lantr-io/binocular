package binocular.watchtower

import scalus.uplc.builtin.*
import scalus.uplc.builtin.Data.{FromData, ToData}

// Scalus mirror of ft-bifrost-bridge `lib/bifrost/types/config.ak::ConfigDatum`. Field order is
// positional in the Plutus Constr — keep it identical to the .ak record so `config[N]` reads on the
// bridge validators line up. All hash/policy/asset fields are ByteStrings; min_stake is an Int.
//
// Variant B layout (11 fields): 0 bridged_token_policy_id, 1 bridged_token_asset_name,
// 2 completed_peg_ins_merkle_tree_policy_id, 3 completed_peg_outs_merkle_tree_policy_id,
// 4 peg_in_withdraw_script_hash, 5 peg_out_withdraw_script_hash, 6 peg_in_close_verifier_script_hash,
// 7 legit_TM_and_peg_out_produced_verifier_script_hash,
// 8 legit_TM_and_peg_out_not_produced_verifier_script_hash, 9 min_stake, 10 update_auth. The cpi/cpo
// NFT asset names are constants ("CPI"/"CPO"), so no asset-name fields are stored. Index 6 is the
// peg-in CLOSE verifier hash: peg_in.ak's Cancel branch delegates the F4/F5 close checks to a
// withdrawal from this script. It's a runtime config field, so the close verifier can be deployed +
// wired via a config update with no peg_in recompile / PIR re-mint. Dummy (Cancel disabled) until
// the F1–F6 failure-mode milestone ships. Index 10 is the config Update/Retire authority
// (Aiken `Option<AuthorizationMethod>`, None = permanently frozen). Index 11 is the initial
// Bitcoin treasury outpoint (36 bytes: txid internal order ++ vout LE) the FIRST Treasury
// Movement must spend; subsequent TMs chain from the previous Confirmed TM record. Indices 12–16 are
// the operational-parameter tunables appended after the anchor (spec §Operational parameters): 12
// fee_rate_sat_per_vb, 13 per_pegout_fee, 14 min_peg_out_fbtc, 15 leader_reward, 16 schedule (nested
// ScheduleParams). These are OFF-CHAIN consensus anchors / pinned-copy sources — no Aiken validator
// reads a current value; the TM validator is the Scalus contract here, and the leader_reward pin is
// enforced there when N9 lands. Must mirror ft `config.ak::ConfigDatum` (17 fields), because
// `config.config`'s genesis path full-casts the datum, so deploy-bridge must write all of them.
case class ConfigDatum(
    bridgedTokenPolicyId: ByteString,
    bridgedTokenAssetName: ByteString,
    completedPegInsMerkleTreePolicyId: ByteString,
    completedPegOutsMerkleTreePolicyId: ByteString,
    pegInWithdrawScriptHash: ByteString,
    pegOutWithdrawScriptHash: ByteString,
    pegInCloseVerifierScriptHash: ByteString,
    legitTmAndPegOutProducedVerifierScriptHash: ByteString,
    legitTmAndPegOutNotProducedVerifierScriptHash: ByteString,
    minStake: BigInt,
    updateAuth: scalus.cardano.onchain.plutus.prelude.Option[AuthorizationMethod],
    initialBtcTreasuryUtxo: ByteString,
    feeRateSatPerVb: BigInt,
    perPegoutFee: BigInt,
    minPegOutFbtc: BigInt,
    leaderReward: BigInt,
    schedule: ScheduleParams
) derives FromData,
      ToData

// Scalus mirror of `config.ak::ScheduleParams` — the tunable epoch/TM schedule (all Int slot
// values, spec §TM batches and the protocol schedule). Positional; keep field order identical to
// the .ak record. Off-chain readers only.
case class ScheduleParams(
    dkgR1Deadline: BigInt,
    dkgR2Deadline: BigInt,
    updateYDeadline: BigInt,
    tmBatchInterval: BigInt,
    signR1Window: BigInt,
    signR2Window: BigInt,
    leaderSlotT: BigInt,
    tmRecoveryWindow: BigInt,
    finalTmCutoff: BigInt,
    stabilityWindow: BigInt
) derives FromData,
      ToData

// Scalus mirror of `completed-peg-ins-merkle-tree.ak::CompletedPegInsMerkleTreeDatum`. The MPF root
// of completed peg-ins; minted with the empty root (32 zero bytes), then spent+recreated with each
// peg-in inserted on completion.
case class CompletedPegInsMerkleTreeDatum(
    root: ByteString
) derives FromData,
      ToData

// Spend redeemer for `completed-peg-ins-merkle-tree.ak::SpendRedeemer`. The spend handler reads
// config[4] = peg_in_withdraw_script_hash, then requires the peg_in withdraw redeemer at
// `pegInWithdrawRedeemerIndex` to be a `Withdraw(Script(peg_in_withdraw_hash))` carrying a
// CompletePegIn action, and that a withdrawal from that script is present. Both indices are
// computed from the assembled tx (see PegInCompleteTx).
case class CompletedPegInsSpendRedeemer(
    configRefInputIndex: BigInt,
    pegInWithdrawRedeemerIndex: BigInt
) derives FromData,
      ToData

// Mint redeemer for `bridged-token.ak::MintRedeemer` (the fBTC/fSAT policy). The policy reads the
// ConfigDatum from the config ref input at `configRefInputIndex` and enforces the mint/burn rules
// against the peg-in / peg-out withdrawals directly (Variant B – no separate mint checker).
case class BridgedTokenMintRedeemer(
    configRefInputIndex: BigInt
) derives FromData,
      ToData
