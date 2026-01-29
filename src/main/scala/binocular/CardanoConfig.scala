package binocular

import scalus.cardano.address.Network
import scalus.cardano.node.{BlockchainProvider, BlockfrostProvider}
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

/** Configuration for Cardano network connection
  *
  * Supports Blockfrost backend only. Koios and Ogmios are no longer supported.
  *
  * Example usage:
  * {{{
  * // Load from config file or environment variables
  * val config = CardanoConfig.load() match {
  *   case Right(cfg) => cfg
  *   case Left(error) => sys.error(s"Config error: $error")
  * }
  *
  * // Get BlockchainProvider
  * val provider = config.createBlockchainProvider() match {
  *   case Right(p) => p
  *   case Left(error) => sys.error(s"Provider error: $error")
  * }
  * }}}
  */
case class CardanoConfig(
    backend: CardanoBackend,
    network: CardanoNetwork,
    blockfrost: BlockfrostConfig,
    yaci: Option[YaciConfig] = None
) {

    /** Create BlockchainProvider based on configured backend type */
    def createBlockchainProvider()(using
        ec: ExecutionContext
    ): Either[String, BlockchainProvider] = {
        backend match {
            case CardanoBackend.Blockfrost =>
                if blockfrost.projectId.isEmpty || blockfrost.projectId == "changeme" then {
                    Left(
                      "Blockfrost backend requires valid project ID. Set BLOCKFROST_PROJECT_ID or configure binocular.cardano.blockfrost.project-id"
                    )
                } else {
                    Try {
                        val future: Future[BlockfrostProvider] = network match {
                            case CardanoNetwork.Mainnet =>
                                BlockfrostProvider.mainnet(blockfrost.projectId)
                            case CardanoNetwork.Preprod =>
                                BlockfrostProvider.preprod(blockfrost.projectId)
                            case CardanoNetwork.Preview =>
                                BlockfrostProvider.preview(blockfrost.projectId)
                            case CardanoNetwork.Testnet =>
                                BlockfrostProvider.preview(blockfrost.projectId)
                        }
                        Await.result(future, 30.seconds)
                    } match {
                        case Success(provider) => Right(provider)
                        case Failure(ex) =>
                            Left(s"Failed to create Blockfrost provider: ${ex.getMessage}")
                    }
                }
        }
    }

    /** Get Scalus Network for this configuration */
    def scalusNetwork: Network = network match {
        case CardanoNetwork.Mainnet => Network.Mainnet
        case CardanoNetwork.Preprod => Network.Testnet
        case CardanoNetwork.Preview => Network.Testnet
        case CardanoNetwork.Testnet => Network.Testnet
    }

    /** Validate configuration */
    def validate(): Either[String, Unit] = {
        backend match {
            case CardanoBackend.Blockfrost =>
                if blockfrost.projectId.isEmpty || blockfrost.projectId == "changeme" then {
                    Left("Blockfrost project ID must be configured")
                } else {
                    Right(())
                }
        }
    }

    override def toString: String = {
        val maskedBlockfrostId =
            if blockfrost.projectId.length > 8 then blockfrost.projectId.take(8) + "***"
            else "***"

        s"CardanoConfig(backend=$backend, network=$network, blockfrost=BlockfrostConfig($maskedBlockfrostId))"
    }

}

/** Cardano backend types */
enum CardanoBackend {
    case Blockfrost
}

object CardanoBackend {
    def fromString(s: String): Either[String, CardanoBackend] = s.toLowerCase match {
        case "blockfrost" => Right(Blockfrost)
        case _            => Left(s"Unknown Cardano backend: $s. Valid options: blockfrost")
    }
}

/** Cardano network types */
enum CardanoNetwork {
    case Mainnet
    case Preprod
    case Preview
    case Testnet
}

object CardanoNetwork {
    def fromString(s: String): Either[String, CardanoNetwork] = s.toLowerCase match {
        case "mainnet" => Right(Mainnet)
        case "preprod" => Right(Preprod)
        case "preview" => Right(Preview)
        case "testnet" => Right(Testnet)
        case _ =>
            Left(s"Unknown Cardano network: $s. Valid options: mainnet, preprod, preview, testnet")
    }
}

/** Blockfrost backend configuration */
case class BlockfrostConfig(
    projectId: String
)

case class YaciConfig(
    apiUrl: String,
    fetchSlotConfig: Boolean = true
)

object CardanoConfig {

    /** Load configuration from environment variables
      *
      * Environment variables:
      *   - CARDANO_BACKEND: blockfrost (default: blockfrost)
      *   - CARDANO_NETWORK: mainnet, preprod, preview, or testnet (default: mainnet)
      *   - BLOCKFROST_PROJECT_ID: Blockfrost project ID
      */
    def fromEnv(): Either[String, CardanoConfig] = {
        val backendStr = sys.env.getOrElse("CARDANO_BACKEND", "blockfrost")
        val networkStr = sys.env.getOrElse("CARDANO_NETWORK", "mainnet")

        for {
            backend <- CardanoBackend.fromString(backendStr)
            network <- CardanoNetwork.fromString(networkStr)
        } yield {
            CardanoConfig(
              backend = backend,
              network = network,
              blockfrost = BlockfrostConfig(
                projectId = sys.env.getOrElse("BLOCKFROST_PROJECT_ID", "changeme")
              )
            )
        }
    }

    /** Load configuration from application.conf file
      *
      * Reads from binocular.cardano configuration section.
      */
    def fromConfig(config: Config = ConfigFactory.load()): Either[String, CardanoConfig] = {
        Try {
            val cardanoConfig = config.getConfig("binocular.cardano")

            val backendStr = cardanoConfig.getString("backend")
            val networkStr = cardanoConfig.getString("network")

            for {
                backend <- CardanoBackend.fromString(backendStr)
                network <- CardanoNetwork.fromString(networkStr)
            } yield {
                CardanoConfig(
                  backend = backend,
                  network = network,
                  blockfrost = BlockfrostConfig(
                    projectId = cardanoConfig.getString("blockfrost.project-id")
                  )
                )
            }
        } match {
            case Success(result) => result
            case Failure(ex) =>
                Left(s"Failed to load Cardano config from application.conf: ${ex.getMessage}")
        }
    }

    /** Load configuration with priority: environment variables > application.conf
      *
      * Tries environment variables first, falls back to application.conf.
      */
    def load(): Either[String, CardanoConfig] = {
        // Try environment variables first
        val envConfig = fromEnv()

        // If env config has a valid backend configuration, use it
        envConfig match {
            case Right(cfg)
                if cfg.backend == CardanoBackend.Blockfrost &&
                    cfg.blockfrost.projectId != "changeme" =>
                Right(cfg)
            case _ =>
                // Fall back to config file
                fromConfig()
        }
    }

    /** Create Blockfrost configuration for a specific network
      *
      * @param network
      *   Cardano network (mainnet, preprod, preview, testnet)
      * @param projectId
      *   Blockfrost project ID
      */
    def blockfrost(
        network: CardanoNetwork,
        projectId: String
    ): CardanoConfig = {
        CardanoConfig(
          backend = CardanoBackend.Blockfrost,
          network = network,
          blockfrost = BlockfrostConfig(projectId)
        )
    }
}
