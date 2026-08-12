package binocular.watchtower

import scalus.uplc.builtin.*
import scalus.uplc.builtin.Data.{FromData, ToData}

// Scalus mirror of ft-bifrost-bridge `lib/bifrost/types/config.ak::ConfigDatum`. Field order is
// positional in the Plutus Constr — keep it identical to the .ak record so `config[N]` reads on the
// bridge validators line up.
//
// Rev-5.5 layout (spec §Config datum, TWELVE fields): 0 update_auth (Aiken
// `Option<AuthorizationMethod>`, None = permanently frozen), 1 params (the nested [[ConfigParams]]
// record), 2 bridged_token_policy, 3 completed_peg_ins_policy (CPI trie NFT policy),
// 4 bridge_state_policy (the singleton NFT policy), 5 tm_script_hash (spec [CFG-2]: the TM
// validator hash = TM NFT policy id, published so off-chain readers can locate the TM address
// without a build-time constant — NO on-chain reader), 6 peg_in_script_hash, 7 peg_out_script_hash,
// 8 spo_bans_policy_id, 9 spos_registry_policy_id, 10 treasury_info_policy_id, 11 y_federation.
//
// TWO RULES DECIDE WHERE A FIELD GOES, and rev 5.5 renumbered everything to obey them:
//   [CFG-6] an identity or a KEY is top-level; a tunable NUMBER lives inside `params`. That is why
//     `y_federation` sits at 11 while `federation_csv_blocks` sits inside `params`, and why the
//     three ban-schedule numbers moved out of the datum body into `params`.
//   [CFG-5] a new field is APPENDED at the tail, never inserted, and `params` sits at index 1 and
//     never moves. The rev-5.4 layout put `params` LAST and told the reader to append after it,
//     which invites exactly the edit that breaks every index: insert before `params` to keep it
//     last. At index 1 there is no "last" property left to preserve.
//
// Fields 8-11 are the federation identity (spec [CFG-3]): published so an SPO configures none of
// them. Every one is an INPUT to the policy id it identifies, so a node cannot derive the address
// it would read them from — and one wrong input yields a well-formed address holding NOTHING rather
// than an error. `spos_registry_policy_id` gained an ON-CHAIN reader in rev 5.5 ([PRE-4]:
// `treasury.ak` gates its RegistryUpdate branch on it); the rest stay off-chain discovery.
//
// `y_federation` moved here from TreasuryDatum in rev 5.5: nothing rotated it there, and here an
// ordinary Config Update rotates it, which is the federation-key rotation the field-permission
// matrix always promised ([UY-5] reads it on-chain).
//
// Removed in rev 5.5: treasury_info_asset_name — the Treasury state NFT name is the [CFG-4]
// constant `"BFRTRY"` ([[TreasuryInfoContract.AssetName]]), so the field had nothing left to say.
// Removed earlier, from rev 5.1: the bridged-token asset name (now the [CFG-1] constant
// [[ConfigDatum.BridgedTokenAssetName]]), the peg-in close verifier, both legit_TM verifier hashes,
// min_stake, initial_btc_treasury_utxo and leader_reward.
//
// Must mirror ft `config.ak::ConfigDatum` exactly, because `config.config`'s genesis path
// full-casts the datum, so deploy-bridge must write all twelve.
case class ConfigDatum(
    updateAuth: scalus.cardano.onchain.plutus.prelude.Option[AuthorizationMethod],
    params: ConfigParams,
    bridgedTokenPolicy: ByteString,
    completedPegInsPolicy: ByteString,
    bridgeStatePolicy: ByteString,
    tmScriptHash: ByteString,
    pegInScriptHash: ByteString,
    pegOutScriptHash: ByteString,
    spoBansPolicyId: ByteString,
    sposRegistryPolicyId: ByteString,
    treasuryInfoPolicyId: ByteString,
    yFederation: ByteString
) derives FromData,
      ToData

object ConfigDatum {

    /** The bridged-token (fBTC) asset name — spec [CFG-1]: a protocol constant, not a Config field.
      * Mirrors `lib/bifrost/constants.ak::bridged_token_asset_name`.
      */
    val BridgedTokenAssetName: ByteString = ByteString.fromString("fSAT")

}

// Scalus mirror of `config.ak::ConfigParams` — every value with no on-chain reader (spec [CFG-6]),
// nested as ConfigDatum field 1. Positional; keep field order identical to the .ak record:
// 0 schedule, 1 fee_rate_sat_per_vb, 2 per_pegout_fee, 3 min_peg_out_fbtc, 4 base_ban_duration_ms,
// 5 max_faults_before_permanent, 6 max_validity_window_ms, 7 federation_csv_blocks.
//
// `schedule` moved from index 3 to index 0 in rev 5.5 and never moves again, for the same reason
// `params` sits at ConfigDatum index 1. The three ban-schedule numbers moved IN from the datum body
// (they mirror what spo_bans was parameterized with; the ApplyBan builder computes a ban's end time
// from the first two and bounds its validity interval by the third), and `federation_csv_blocks`
// joined them — a block count, so [CFG-6] puts it here rather than next to `y_federation`.
//
// Governance replaces the record wholesale, which is what nesting buys.
case class ConfigParams(
    schedule: ScheduleParams,
    feeRateSatPerVb: BigInt,
    perPegoutFee: BigInt,
    minPegOutFbtc: BigInt,
    baseBanDurationMs: BigInt,
    maxFaultsBeforePermanent: BigInt,
    maxValidityWindowMs: BigInt,
    federationCsvBlocks: BigInt
) derives FromData,
      ToData

// Scalus mirror of `config.ak::ScheduleParams` — the tunable epoch/TM schedule (all Int slot
// values, spec §TM batches and the protocol schedule), nested as ConfigParams field 0.
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
