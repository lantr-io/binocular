package binocular

import pureconfig.*

/** Configuration for the TMTx relay command.
  *
  * Loaded from application-preprod.conf or env vars.
  */
case class RelayConfig(
    tmtxPolicyId: String = "186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4",
    tmtxAssetName: String = "TMTx",
    pollInterval: Int = 5,
    retryInterval: Int = 10
) derives ConfigReader
