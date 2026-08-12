package binocular

import org.scalatest.funsuite.AnyFunSuite
import pureconfig.ConfigSource

/** `reference.conf` is the ONLY source of defaults for [[BinocularConfig]].
  *
  * PureConfig's Scala 3 derivation does not read a case class's default arguments, so a field
  * added to (say) [[binocular.watchtower.BridgeConfig]] with a Scala default and no matching key
  * in `reference.conf` becomes a REQUIRED key. Every existing config file then fails to load —
  * including the deployed watchtower's, which finds out at startup:
  *
  * {{{
  * Configuration error: Key not found: 'y-federation-hex'., Key not found: 'federation-csv-blocks'.
  * }}}
  *
  * That is exactly what shipped with the rev-5.5 treasury refactor. This test loads the packaged
  * `reference.conf` alone — no application.conf, no env — so a missing key fails here instead.
  */
class ReferenceConfTest extends AnyFunSuite {

    test("reference.conf alone satisfies every required BinocularConfig key") {
        val loaded = ConfigSource.defaultReference
            .at("binocular")
            .load[BinocularConfig]
        assert(
          loaded.isRight,
          s"reference.conf is missing a key for a config field: ${loaded.left.toOption.map(_.prettyPrint()).getOrElse("")}"
        )
    }

    test("the rev-5.5 federation keys resolve to their documented defaults") {
        val cfg = ConfigSource.defaultReference.at("binocular").loadOrThrow[BinocularConfig]
        // No safe default: deploy-bridge rejects an empty key rather than derive a treasury
        // address that holds nothing.
        assert(cfg.bridge.yFederationHex.isEmpty)
        assert(cfg.bridge.federationCsvBlocks == 144)
    }
}
