package binocular.watchtower

import pureconfig.*

/** Configuration for the ft-bifrost-bridge contracts the watchtower interacts with.
  *
  * `configNftPolicyId` / `configNftAssetName` only affect the peg_in_validator's script hash; the
  * mint path does not read the config-NFT UTxO (only Cancel / CompletePegIn do), so the defaults
  * are placeholders sufficient for minting a PegInRequest.
  *
  * Loaded by PureConfig from reference.conf / application.conf / env vars.
  */
case class BridgeConfig(
    plutusJson: String = "../../FluidTokens/ft-bifrost-bridge/onchain/plutus.json",
    configNftPolicyId: String = "00000000000000000000000000000000000000000000000000000000",
    configNftAssetName: String = ""
) derives ConfigReader
