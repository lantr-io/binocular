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
// Fields 8-11 are the federation identity (spec [CFG-3]): published so an SPO configures none of
// them. Every policy id here is an INPUT to the address it identifies, so a node cannot derive the
// address it would read them from — and one wrong input yields a well-formed address holding
// NOTHING rather than an error.
//
// `params` is at index 1 ([CFG-5]). Rev 5.4 put it LAST and told the reader to append after it,
// which invites the one edit that shifts every index: insert before `params` to keep it last. At
// index 1 there is no "last" property left to preserve.
//
// [CFG-6] decides placement: an identity or a key is a top-level field, a tunable number lives
// inside `params`. That is why `yFederation` is #11 while `federationCsvBlocks` is params[7].
//
// Gone from rev 5.1: the bridged-token asset name (now the [CFG-1] constant
// [[ConfigDatum.BridgedTokenAssetName]]), the peg-in close verifier, both legit_TM verifier
// hashes, min_stake, initial_btc_treasury_utxo and leader_reward.
// Gone in rev 5.5: treasuryInfoAssetName — the Treasury state NFT name is the [CFG-4] constant
// [[ConfigDatum.TreasuryInfoAssetName]].
//
// Must mirror ft `config.ak::ConfigDatum` (12 fields), because `config.config`'s genesis path
// full-casts the datum, so deploy-bridge must write all of them.
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

    /** The Config NFT asset name — spec [CFG-7]: a protocol constant, not a validator parameter.
      * Mirrors `lib/bifrost/constants.ak::config_nft_asset_name`.
      *
      * Rev 5.5 removed it from five validators' parameter lists. A constant in one script beside a
      * parameter in five has no safe failure mode: one divergent deployment argument leaves
      * `treasury.ak` searching for a token that does not exist, and its `Retire` branch fails with
      * it, so the Treasury state UTxO can never be spent again.
      */
    val ConfigNftAssetName: ByteString = ByteString.fromString("BIFCFG")

    /** The Treasury state NFT asset name — spec [CFG-4]: a protocol constant, not Config #13.
      * Mirrors `lib/bifrost/constants.ak::treasury_info_nft_asset_name`.
      *
      * Uniqueness comes from the one-shot outpoint baked into the policy id, so the name had
      * nothing left to say.
      */
    val TreasuryInfoAssetName: ByteString = ByteString.fromString("BFRTRY")

}

// Scalus mirror of `config.ak::ConfigParams` — every value with NO on-chain reader, nested as
// ConfigDatum field 1 (spec §Config datum). Positional; keep field order identical to the .ak
// record: 0 schedule, 1 fee_rate_sat_per_vb, 2 per_pegout_fee, 3 min_peg_out_fbtc,
// 4 base_ban_duration_ms, 5 max_faults_before_permanent, 6 max_validity_window_ms,
// 7 federation_csv_blocks.
//
// `schedule` is at index 0 for the same reason `params` is at ConfigDatum index 1: a nested record
// at the tail invites an append that shifts it.
case class ConfigParams(
    schedule: ScheduleParams,
    feeRateSatPerVb: BigInt,
    perPegoutFee: BigInt,
    minPegOutFbtc: BigInt,
    baseBanDurationMs: BigInt,
    maxFaultsBeforePermanent: BigInt,
    maxValidityWindowMs: BigInt,
    federationCsvBlocks: BigInt,
    // params[8], spec [CFG-9]: the CSV delay of the peg-in tree's DEPOSITOR REFUND leaf.
    // Beside federationCsvBlocks because the two do the same job — both are block counts
    // hashed into the peg-in Taproot, so both decide the deposit ADDRESS and both must be
    // unanimous across SPOs. It was the last of that tree's four inputs left in each
    // operator's own file, where a disagreement split the federation in silence.
    peginRefundTimeoutBlocks: BigInt
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
