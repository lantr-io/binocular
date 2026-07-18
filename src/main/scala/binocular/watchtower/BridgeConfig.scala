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
    // The TM-control NFT (one-shot) that authenticates the control UTxO whose TmControlDatum names
    // the key authorized to mint the TM NFT. These parameterize TreasuryMovementValidator (so they
    // fix the TM script hash / address) and are STABLE — the address does NOT depend on the
    // authorized key, which lives in the control datum and can rotate freely. policy = 28-byte hex;
    // name = hex asset name. Empty = placeholder (until the control UTxO is deployed).
    tmControlNftPolicy: String = "",
    tmControlNftName: String = "",
    // The 28-byte pubkey-hash written into the TM-control datum at deploy = the key authorized to
    // mint TM NFTs (the SPO/poster key — on the devnet, heimdall's Cardano signing key; get it from
    // `heimdall wallet-address`). Default for deploy-bridge's --authorized-minter.
    tmAuthorizedMinter: String = "",
    // The completed-peg-outs one-shot fixes that validator's params (hence its policyId + NFT asset
    // name); peg-out-complete needs it to reconstruct the script to SPEND the MPF UTxO. `Option`
    // (not `""`): a peg-in-only bridge (e.g. the synced config) simply omits the key — pureconfig
    // maps a missing key to `None`. The peg-out commands fail fast when a required ref is absent.
    //
    // The heavy scripts' CIP-33 reference UTxOs (peg_in, bridged_token, completed_peg_ins, peg_out,
    // completed_peg_outs) are no longer recorded here: deploy-script-refs publishes them to the
    // sponsor wallet, and the completion paths discover them by the `reference_script_hash` each
    // carries (see CommandHelpers.refScriptUtxosByHash). A script hash not found on-chain falls back
    // to inlining the script in the witness set (only viable for small txs).
    completedPegOutsOneShotRef: Option[String] = None
) derives ConfigReader
