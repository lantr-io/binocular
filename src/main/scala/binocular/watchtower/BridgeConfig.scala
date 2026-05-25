package binocular.watchtower

import pureconfig.*

/** Configuration for the ft-bifrost-bridge contracts the watchtower interacts with.
  *
  * `configNftPolicyId` / `configNftAssetName` only affect the peg_in_validator's script hash; the
  * mint path does not read the config-NFT UTxO (only Cancel / CompletePegIn do), so the defaults
  * are placeholders sufficient for minting a PegInRequest.
  *
  * `bridgedTokenPolicyId` / `bridgedTokenAssetName` identify the fBTC (bridged_token) asset. They
  * parameterize [[PegInDepositorAuthValidator]] so it can require that the completion tx actually
  * pays `peg_in_amount` fBTC to the recipient the depositor signed for — the recipient-binding from
  * technical_documentation.md §"Complete peg-in". Placeholders here keep the same provisional shape
  * as the config NFT; both are finalized once the bridge config NFT is deployed (F3), which fixes
  * the real bridged_token policy and forces a re-mint of the PegInRequests under the matching
  * hashes.
  *
  * Loaded by PureConfig from reference.conf / application.conf / env vars.
  */
case class BridgeConfig(
    plutusJson: String = "../../FluidTokens/ft-bifrost-bridge/onchain/plutus.json",
    configNftPolicyId: String = "00000000000000000000000000000000000000000000000000000000",
    configNftAssetName: String = "",
    bridgedTokenPolicyId: String = "00000000000000000000000000000000000000000000000000000000",
    bridgedTokenAssetName: String = "6642544300000000", // "fBTC\0\0\0\0" placeholder
    // The one-shot wallet UTxO (TX_HASH#INDEX) consumed when the completed-peg-ins MPF NFT was
    // minted in deploy-bridge (F3). It fixes that validator's parameter set, hence its script hash
    // (= policyId) and NFT asset name = hash_output_ref(one_shot). pegin-complete needs it to
    // reconstruct the script in order to SPEND the MPF UTxO. The config NFT, by contrast, is only a
    // reference input, so it is located by its NFT and needs no script. Empty until F3 is deployed.
    completedPegInsOneShotRef: String = "",
    // Pubkey-hash (28-byte hex) of the authority allowed to mint the TM NFT (the SPO/poster key that
    // signs the Post-TM tx — on the devnet, heimdall's Cardano wallet key). Parameterizes
    // TreasuryMovementValidator, so it also affects the TM script hash / address. Must match the key
    // heimdall signs Post-TM with. Empty = placeholder (mint cannot be authorized).
    tmAuthorityPkh: String = ""
) derives ConfigReader
