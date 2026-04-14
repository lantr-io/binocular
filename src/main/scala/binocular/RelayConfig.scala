package binocular

import pureconfig.*

/** Configuration for the TMTx relay command.
  *
  * Loaded from application-preprod.conf or env vars.
  */
case class RelayConfig(
    tmtxPolicyId: String = "eafdc4d9733275d3e06cfe5fe54a13fff3ba5baa8d65636260537907",
    tmtxAssetName: String = "TMTx",
    pollInterval: Int = 5,
    retryInterval: Int = 10
) derives ConfigReader
