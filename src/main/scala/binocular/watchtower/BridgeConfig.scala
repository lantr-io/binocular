package binocular.watchtower

import pureconfig.*

/** Configuration for the ft-bifrost-bridge contracts the watchtower interacts with.
  *
  * `configNftPolicyId` / `configNftAssetName` only affect the peg_in_validator's script hash; the
  * mint path does not read the config-NFT UTxO (only Cancel / CompletePegIn do), so the defaults
  * are placeholders sufficient for minting a PegInRequest.
  *
  * `bridgedTokenPolicyId` / `bridgedTokenAssetName` identify the fBTC (bridged_token) asset. The
  * completion tx mints this asset and binds it to the recipient the depositor signed for — the
  * recipient-binding from technical_documentation.md §"Complete peg-in", now enforced inside
  * `peg_in.ak` itself (B1; the standalone depositor-auth withdraw was removed). Placeholders here
  * keep the same provisional shape as the config NFT; both are finalized once the bridge config NFT
  * is deployed (F3), which fixes the real bridged_token policy and forces a re-mint of the
  * PegInRequests under the matching hashes.
  *
  * Loaded by PureConfig from reference.conf / application.conf / env vars.
  */
/** The `spo_bans` ban schedule, fixed at bridge genesis.
  *
  * `base_ban_duration_ms * 2^(n-1)` is the nth ban's length; a pool is banned permanently at
  * `max_faults_before_permanent`; `max_validity_window_ms` bounds an ApplyBan tx's validity
  * interval. Defaults are the preprod values.
  */
case class BanScheduleConfig(
    baseBanDurationMs: Long = 600000L,
    maxFaultsBeforePermanent: Long = 3L,
    maxValidityWindowMs: Long = 3600000L
)

case class BridgeConfig(
    plutusJson: String = "../../FluidTokens/ft-bifrost-bridge/onchain/plutus.json",
    configNftPolicyId: String = "00000000000000000000000000000000000000000000000000000000",
    configNftAssetName: String = "",
    bridgedTokenPolicyId: String = "00000000000000000000000000000000000000000000000000000000",
    bridgedTokenAssetName: String = "66534154", // "fSAT" placeholder
    // The one-shot wallet UTxO (TX_HASH#INDEX) consumed when the completed-peg-ins MPF NFT was
    // minted in deploy-bridge (F3). It fixes that validator's parameter set, hence its script hash
    // (= policyId) and NFT asset name = hash_output_ref(one_shot). pegin-complete needs it to
    // reconstruct the script in order to SPEND the MPF UTxO. The config NFT, by contrast, is only a
    // reference input, so it is located by its NFT and needs no script. Empty until F3 is deployed.
    completedPegInsOneShotRef: String = "",
    // The initial Bitcoin treasury outpoint written into config field 11 (initial_btc_treasury_utxo)
    // at deploy, in display form "TXID:VOUT" (TXID as shown by explorers; converted to internal
    // byte order internally). The FIRST Treasury Movement must spend this outpoint; every
    // subsequent TM chains from the previous Confirmed TM record.
    initialBtcTreasuryUtxo: String = "",
    // The DKG candidate-set stake threshold written into config field 9 at deploy (lovelace): a
    // pool's epoch-snapshot active_stake must reach it to register / enter the candidate set.
    // Read off-chain only (heimdall's register_spo R2 gate). 0 = no threshold, which is what every
    // bridge deployed before this key existed carries — raise it with `update-config --min-stake`
    // rather than by editing each SPO's own config, or the operators disagree about who is eligible.
    minStakeLovelace: Long = 0L,
    // Operational-parameter tunables written into config fields 12–15 at deploy (off-chain
    // consensus anchors; the schedule #16 uses spec devnet defaults, replaced wholesale by a
    // governance Update). All five are governance-updatable in place — `update-config --fee-rate`
    // et al — which is how the bridge tracks the Bitcoin fee market. See ConfigDatum #12–16.
    feeRateSatPerVb: Long = 1L,
    perPegoutFeeSat: Long = 1000L,
    minPegOutSat: Long = 10000L,
    leaderRewardLovelace: Long = 2000000L,
    // The ban schedule baked into the spo_bans policy id at genesis, and published verbatim as
    // config #18-20. Unlike the tunables above these are NOT governance-updatable in place: they
    // are inputs to the policy hash, so changing one names a different ban list — which is exactly
    // why they are chosen once, here, and then read by every SPO rather than typed by each.
    banSchedule: BanScheduleConfig = BanScheduleConfig(),
    // The completed-peg-outs one-shot fixes that validator's params (hence its policyId + NFT asset
    // name); peg-out-complete needs it to reconstruct the script to SPEND the MPF UTxO. `Option`
    // (not `""`): a peg-in-only bridge (e.g. the synced config) simply omits the key — pureconfig
    // maps a missing key to `None`. The peg-out commands fail fast when a required ref is absent.
    //
    // REQUIRED BY `confirm-tmtx` since the peg-out trie v2 change (2026-07). Every TM Confirm tx
    // spends and recreates the completed-peg-outs trie UTxO, so the confirm daemon must rebuild that
    // validator too — its other parameter is the TM script hash, which confirm already derives.
    // `confirm-tmtx` (and therefore `watchtower`) exits at startup when this key is missing, because
    // no TM can be confirmed without the trie spend. A peg-in-only deployment must still set it.
    //
    // The heavy scripts' CIP-33 reference UTxOs (peg_in, bridged_token, completed_peg_ins, peg_out,
    // completed_peg_outs) are no longer recorded here: deploy-script-refs publishes them to the
    // sponsor wallet, and the completion paths discover them by the `reference_script_hash` each
    // carries (see CommandHelpers.refScriptUtxosByHash). A script hash not found on-chain falls back
    // to inlining the script in the witness set (only viable for small txs).
    completedPegOutsOneShotRef: Option[String] = None,
    // --- POR sweeper (spec rev 5.2) ---
    // Chain peg-out Complete after TM Confirm: after each confirmed TM the watchtower burns the
    // locked fBTC of every PAID PegOutRequest and keeps its MIN_ADA. ON by default — completion is
    // permissionless cleanup that pays for itself, and without it paid requests accumulate forever.
    // Turn it off only to run a watchtower that confirms but never spends on completions.
    porSweeper: Boolean = true,
    // Seconds between sweeps when nothing was confirmed. A sweep runs IMMEDIATELY after a confirm
    // (that is the whole point of chaining); this interval only governs the idle path, which exists
    // to pick up requests paid by a TM another party confirmed.
    porSweepIntervalSeconds: Int = 300,
    // Directory holding the persistent completed-peg-outs trie mirror (`cpo-trie.json`). Losing it
    // is not fatal — the sweeper reconstructs from chain history — but reconstruction reads the full
    // history of two addresses, so it should live on durable storage. `~` is expanded.
    stateDir: String = ".binocular"
) derives ConfigReader

object BridgeConfig {

    /** "TXID:VOUT" (display txid) -> 36-byte outpoint (txid internal order ++ vout LE). */
    def outpointFromDisplay(s: String): scalus.uplc.builtin.ByteString = {
        val parts = s.split(':')
        require(parts.length == 2, s"expected TXID:VOUT, got '$s'")
        val txidHex = parts(0)
        require(txidHex.length == 64, s"txid must be 64 hex chars: $txidHex")
        val txidInternal = txidHex.grouped(2).toSeq.reverse.mkString
        val vout = parts(1).toLong
        require(vout >= 0 && vout <= 0xffffffffL, s"vout out of range: $vout")
        val voutLe =
            f"${vout & 0xff}%02x${(vout >> 8) & 0xff}%02x${(vout >> 16) & 0xff}%02x${(vout >> 24) & 0xff}%02x"
        scalus.uplc.builtin.ByteString.fromHex(txidInternal + voutLe)
    }
}
