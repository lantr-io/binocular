package binocular.watchtower

import binocular.*

import pureconfig.*

/** Configuration for the TM relay/confirm daemons.
  *
  * Loaded from application-preprod.conf or env vars.
  */
case class RelayConfig(
    pollInterval: Int = 5,
    retryInterval: Int = 10,
    // BTC txids (display / big-endian hex) of Unconfirmed TM UTxOs to skip in confirm-tmtx. Use for
    // permanently-dead TMs whose signed BTC tx can never be mined — e.g. a superseded treasury
    // handoff whose input was already spent by a competing TM. Their PIR-less TM UTxO lingers at the
    // validator and would otherwise be re-scanned (and its BTC tx re-looked-up) every run.
    skipBtcTxids: List[String] = Nil
) derives ConfigReader
