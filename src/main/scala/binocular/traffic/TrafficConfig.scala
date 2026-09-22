package binocular.traffic

import binocular.BinocularConfig
import binocular.bitcoin.BitcoinNetwork
import binocular.oracle.CardanoNetwork
import binocular.watchtower.ScheduleParams
import pureconfig.ConfigReader
import scala.concurrent.duration.*

/** Demo traffic only. Invalid settings stop traffic, not the other watchtower workers. Fee policy
  * and the tick interval are constants ([TRF-120]); the live Config supplies every bridge limit.
  */
final case class TrafficConfig(
    enabled: Boolean = false,
    virtualEpochSlots: Long = 86400,
    maxAmountSat: Long = 50000,
    meanInterval: FiniteDuration = 4.hours
) derives ConfigReader {

    def validateSchedule(params: ScheduleParams): Either[String, Unit] =
        for {
            start <- TrafficSchedule.at(0, virtualEpochSlots, params)
            earliest <- TrafficSchedule.at(start.updateYDeadline.toLong, virtualEpochSlots, params)
            _ <- Either.cond(
              earliest.nextBatch.nonEmpty && earliest.finalCutoff - earliest.tipSlot >= 21600,
              (),
              "Traffic cycle has no deposit window: match virtual-epoch-slots to Heimdall and the live bridge schedule"
            )
        } yield ()

    /** [TRF-121], except the live `min_peg_out_fbtc` bound, which needs the Config UTxO. */
    def validate(config: BinocularConfig): Either[String, Unit] =
        for {
            network <- CardanoNetwork.fromString(config.cardano.network)
            _ <- Either.cond(
              network != CardanoNetwork.Mainnet &&
                  config.bitcoinNode.bitcoinNetwork != BitcoinNetwork.Mainnet,
              (),
              "Traffic requires Bitcoin and Cardano test networks"
            )
            _ <- Either.cond(
              maxAmountSat > 0 && virtualEpochSlots > 0,
              (),
              "Invalid traffic limits"
            )
            _ <- Either.cond(
              meanInterval >= TrafficPlan.TickSeconds.seconds,
              (),
              s"mean-interval must be at least ${TrafficPlan.TickSeconds} seconds"
            )
        } yield ()
}
