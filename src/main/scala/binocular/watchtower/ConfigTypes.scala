package binocular.watchtower

import scalus.uplc.builtin.*
import scalus.uplc.builtin.Data.{FromData, ToData}

// Scalus mirror of ft-bifrost-bridge `lib/bifrost/types/config.ak::ConfigDatum`. Field order is
// positional in the Plutus Constr — keep it identical to the .ak record so `config[N]` reads on the
// bridge validators line up.
//
// Rev-5.4 layout (spec §Config datum, fifteen fields): 0 update_auth (Aiken
// `Option<AuthorizationMethod>`, None = permanently frozen), 1 bridged_token_policy,
// 2 completed_peg_ins_policy (CPI trie NFT policy), 3 bridge_state_policy (the singleton NFT
// policy), 4 tm_script_hash (spec [CFG-2]: the TM validator hash = TM NFT policy id, published so
// off-chain readers can locate the TM address without a build-time constant — NO on-chain reader),
// 5 peg_in_script_hash, 6 peg_out_script_hash, 7 spo_bans_policy_id, 8 base_ban_duration_ms,
// 9 max_faults_before_permanent, 10 max_validity_window_ms, 11 spos_registry_policy_id,
// 12 treasury_info_policy_id, 13 treasury_info_asset_name, 14 params (the nested [[ConfigParams]]
// record, so governance can replace the tunables wholesale without renumbering their neighbours).
//
// Fields 7-13 are the federation identity (spec [CFG-3]): off-chain readers only, published so an
// SPO configures none of them. Every one is an INPUT to the policy id it identifies, so a node
// cannot derive the address it would read them from — and one wrong input yields a well-formed
// address holding NOTHING rather than an error.
//
// `params` is LAST on purpose: every field before it is a scalar at a frozen index, so the datum
// grows by appending AFTER the nested record instead of pushing it along.
//
// Gone from rev 5.1: the bridged-token asset name (now the [CFG-1] constant
// [[ConfigDatum.BridgedTokenAssetName]]), the peg-in close verifier, both legit_TM verifier
// hashes, min_stake, initial_btc_treasury_utxo and leader_reward.
//
// Must mirror ft `config.ak::ConfigDatum` (15 fields), because `config.config`'s genesis path
// full-casts the datum, so deploy-bridge must write all of them.
case class ConfigDatum(
    updateAuth: scalus.cardano.onchain.plutus.prelude.Option[AuthorizationMethod],
    bridgedTokenPolicy: ByteString,
    completedPegInsPolicy: ByteString,
    bridgeStatePolicy: ByteString,
    tmScriptHash: ByteString,
    pegInScriptHash: ByteString,
    pegOutScriptHash: ByteString,
    spoBansPolicyId: ByteString,
    baseBanDurationMs: BigInt,
    maxFaultsBeforePermanent: BigInt,
    maxValidityWindowMs: BigInt,
    sposRegistryPolicyId: ByteString,
    treasuryInfoPolicyId: ByteString,
    treasuryInfoAssetName: ByteString,
    params: ConfigParams
) derives FromData,
      ToData

object ConfigDatum {

    /** The bridged-token (fBTC) asset name — spec [CFG-1]: a protocol constant, not a Config field.
      * Mirrors `lib/bifrost/constants.ak::bridged_token_asset_name`.
      */
    val BridgedTokenAssetName: ByteString = ByteString.fromString("fSAT")

}

// Scalus mirror of `config.ak::ConfigParams` — the tunable operational parameters, nested as
// ConfigDatum field 14 (spec §Operational parameters). Positional; keep field order identical to
// the .ak record: 0 fee_rate_sat_per_vb, 1 per_pegout_fee, 2 min_peg_out_fbtc, 3 schedule.
// OFF-CHAIN consensus anchors / pinned-copy sources — no Aiken validator reads a current value.
case class ConfigParams(
    feeRateSatPerVb: BigInt,
    perPegoutFee: BigInt,
    minPegOutFbtc: BigInt,
    schedule: ScheduleParams
) derives FromData,
      ToData

// Scalus mirror of `config.ak::ScheduleParams` — the tunable epoch/TM schedule (all Int slot
// values, spec §TM batches and the protocol schedule), nested as ConfigParams field 3.
// Positional; keep field order identical to the .ak record. Off-chain readers only.
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
// config[5] = peg_in_script_hash, then requires the peg_in withdraw redeemer at
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
