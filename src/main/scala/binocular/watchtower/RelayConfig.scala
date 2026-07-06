package binocular.watchtower

import binocular.*
import binocular.bitcoin.*
import binocular.oracle.*

import pureconfig.*

/** Configuration for the TMTx relay command.
  *
  * Loaded from application-preprod.conf or env vars.
  */
case class RelayConfig(
    tmtxPolicyId: String = "eafdc4d9733275d3e06cfe5fe54a13fff3ba5baa8d65636260537907",
    tmtxAssetName: String = "TMTx",
    pollInterval: Int = 5,
    retryInterval: Int = 10,
    // BTC txids (display / big-endian hex) of Unconfirmed TM UTxOs to skip in confirm-tmtx. Use for
    // permanently-dead TMs whose signed BTC tx can never be mined — e.g. a superseded treasury
    // handoff whose input was already spent by a competing TM. Their PIR-less TM UTxO lingers at the
    // validator and would otherwise be re-scanned (and its BTC tx re-looked-up) every run.
    skipBtcTxids: List[String] = Nil
) derives ConfigReader
