package binocular.traffic

import binocular.BinocularConfig
import binocular.bitcoin.BitcoinNodeConfig
import binocular.oracle.{CardanoConfig, OracleConfig, WalletConfig}
import binocular.watchtower.ScheduleParams
import org.scalatest.funsuite.AnyFunSuite
import pureconfig.ConfigSource
import scala.concurrent.duration.*

class TrafficConfigTest extends AnyFunSuite {
    test("startup refuses a virtual epoch too short for the deployed batch schedule") {
        val deployed =
            ScheduleParams(3600, 7200, 10800, 21600, 1800, 1800, 60, 129600, 345600, 129600)
        assert(TrafficConfig().validateSchedule(deployed).isLeft)
        assert(TrafficConfig(virtualEpochSlots = 432000).validateSchedule(deployed).isRight)
        assert(TrafficConfig().validateSchedule(deployed.copy(stabilityWindow = 7200)).isRight)
    }
    private val config = BinocularConfig(
      BitcoinNodeConfig(network = "testnet4"),
      CardanoConfig(network = "preprod"),
      WalletConfig(),
      OracleConfig()
    )

    test("traffic is opt-in and has exactly the four documented keys") {
        val defaults = ConfigSource.default.at("binocular.traffic").loadOrThrow[TrafficConfig]
        assert(defaults == TrafficConfig(false, 86400, 50000, 4.hours))
        assert(defaults.validate(config).isRight)
        val custom = ConfigSource
            .string("binocular.traffic { enabled=true, max-amount-sat=40000, mean-interval=30m }")
            .withFallback(ConfigSource.default)
            .at("binocular.traffic")
            .loadOrThrow[TrafficConfig]
        assert(custom == TrafficConfig(true, 86400, 40000, 30.minutes))
    }

    test("mainnet, a zero amount or epoch, and a mean interval under one tick are refused") {
        assert(
          TrafficConfig()
              .validate(config.copy(bitcoinNode = BitcoinNodeConfig(network = "main")))
              .isLeft
        )
        assert(
          TrafficConfig().validate(config.copy(cardano = CardanoConfig(network = "mainnet"))).isLeft
        )
        for bad <- Seq(
              TrafficConfig(maxAmountSat = 0),
              TrafficConfig(virtualEpochSlots = 0),
              TrafficConfig(meanInterval = 299.seconds)
            )
        do assert(bad.validate(config).isLeft, bad.toString)
        assert(TrafficConfig(meanInterval = 300.seconds).validate(config).isRight)
    }
}
