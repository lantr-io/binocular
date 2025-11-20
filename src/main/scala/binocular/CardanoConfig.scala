package binocular

import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.backend.koios.KoiosBackendService
import com.bloxbean.cardano.client.backend.ogmios.http.OgmiosBackendService
import com.bloxbean.cardano.client.common.model.{Network, Networks}
import com.typesafe.config.{Config, ConfigFactory}

import scala.util.{Failure, Success, Try}

/** Configuration for Cardano network connection
  *
  * Supports multiple backend types:
  *   - Blockfrost: Hosted API service (easy setup, requires API key)
  *   - Koios: Community-run public API (no registration needed)
  *   - Ogmios: Local Cardano node (trustless, maximum security)
  *
  * Example usage:
  * {{{
  * // Load from config file or environment variables
  * val config = CardanoConfig.load() match {
  *   case Right(cfg) => cfg
  *   case Left(error) => sys.error(s"Config error: $error")
  * }
  *
  * // Get BackendService
  * val backendService = config.createBackendService() match {
  *   case Right(service) => service
  *   case Left(error) => sys.error(s"Backend error: $error")
  * }
  * }}}
  */
case class CardanoConfig(
    backend: CardanoBackend,
    network: CardanoNetwork,
    blockfrost: BlockfrostConfig,
    koios: KoiosConfig,
    ogmios: OgmiosConfig,
    yaci: Option[YaciConfig] = None
) {

    /** Create BackendService based on configured backend type */
    def createBackendService(): Either[String, BackendService] = {
        backend match {
            case CardanoBackend.Blockfrost =>
                if blockfrost.projectId.isEmpty || blockfrost.projectId == "changeme" then {
                    Left(
                      "Blockfrost backend requires valid project ID. Set BLOCKFROST_PROJECT_ID or configure binocular.cardano.blockfrost.project-id"
                    )
                } else {
                    Try {
                        new BFBackendService(blockfrost.apiUrl, blockfrost.projectId)
                    } match {
                        case Success(service) => Right(service)
                        case Failure(ex) =>
                            Left(s"Failed to create Blockfrost backend: ${ex.getMessage}")
                    }
                }

            case CardanoBackend.Koios =>
                Try {
                    new KoiosBackendService(koios.apiUrl)
                } match {
                    case Success(service) => Right(service)
                    case Failure(ex) => Left(s"Failed to create Koios backend: ${ex.getMessage}")
                }

            case CardanoBackend.Ogmios =>
                Try {
                    new OgmiosBackendService(ogmios.url)
                } match {
                    case Success(service) => Right(service)
                    case Failure(ex) => Left(s"Failed to create Ogmios backend: ${ex.getMessage}")
                }
        }
    }

    /** Get CCL Network object for this configuration */
    def cclNetwork: Network = network match {
        case CardanoNetwork.Mainnet => Networks.mainnet()
        case CardanoNetwork.Preprod => Networks.preprod()
        case CardanoNetwork.Preview => Networks.preview()
        case CardanoNetwork.Testnet => Networks.testnet()
    }

    /** Validate configuration */
    def validate(): Either[String, Unit] = {
        backend match {
            case CardanoBackend.Blockfrost =>
                if blockfrost.apiUrl.isEmpty then {
                    Left("Blockfrost API URL cannot be empty")
                } else if blockfrost.projectId.isEmpty || blockfrost.projectId == "changeme" then {
                    Left("Blockfrost project ID must be configured")
                } else {
                    Right(())
                }

            case CardanoBackend.Koios =>
                if koios.apiUrl.isEmpty then {
                    Left("Koios API URL cannot be empty")
                } else {
                    Right(())
                }

            case CardanoBackend.Ogmios =>
                if ogmios.url.isEmpty then {
                    Left("Ogmios URL cannot be empty")
                } else {
                    Right(())
                }
        }
    }

    override def toString: String = {
        val maskedBlockfrostId =
            if blockfrost.projectId.length > 8 then blockfrost.projectId.take(8) + "***"
            else "***"

        s"CardanoConfig(backend=$backend, network=$network, blockfrost=BlockfrostConfig(${blockfrost.apiUrl}, $maskedBlockfrostId))"
    }

}

/** Cardano backend types */
enum CardanoBackend {
    case Blockfrost
    case Koios
    case Ogmios
}

object CardanoBackend {
    def fromString(s: String): Either[String, CardanoBackend] = s.toLowerCase match {
        case "blockfrost" => Right(Blockfrost)
        case "koios"      => Right(Koios)
        case "ogmios"     => Right(Ogmios)
        case _ => Left(s"Unknown Cardano backend: $s. Valid options: blockfrost, koios, ogmios")
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
    apiUrl: String,
    projectId: String
)

/** Koios backend configuration */
case class KoiosConfig(
    apiUrl: String
)

/** Ogmios backend configuration */
case class OgmiosConfig(
    url: String
)

case class YaciConfig(
    apiUrl: String,
    fetchSlotConfig: Boolean = true
)

object CardanoConfig {

    /** Load configuration from environment variables
      *
      * Environment variables:
      *   - CARDANO_BACKEND: blockfrost, koios, or ogmios (default: blockfrost)
      *   - CARDANO_NETWORK: mainnet, preprod, preview, or testnet (default: mainnet)
      *   - BLOCKFROST_API_URL: Blockfrost API URL
      *   - BLOCKFROST_PROJECT_ID: Blockfrost project ID
      *   - KOIOS_API_URL: Koios API URL
      *   - OGMIOS_URL: Ogmios WebSocket URL
      */
    def fromEnv(): Either[String, CardanoConfig] = {
        val backendStr = sys.env.getOrElse("CARDANO_BACKEND", "blockfrost")
        val networkStr = sys.env.getOrElse("CARDANO_NETWORK", "mainnet")

        for {
            backend <- CardanoBackend.fromString(backendStr)
            network <- CardanoNetwork.fromString(networkStr)
        } yield {
            val blockfrostApiUrl = sys.env.getOrElse(
              "BLOCKFROST_API_URL",
              network match {
                  case CardanoNetwork.Mainnet => "https://cardano-mainnet.blockfrost.io/api/v0/"
                  case CardanoNetwork.Preprod => "https://cardano-preprod.blockfrost.io/api/v0/"
                  case CardanoNetwork.Preview => "https://cardano-preview.blockfrost.io/api/v0/"
                  case CardanoNetwork.Testnet => "https://cardano-testnet.blockfrost.io/api/v0/"
              }
            )

            val koiosApiUrl = sys.env.getOrElse(
              "KOIOS_API_URL",
              network match {
                  case CardanoNetwork.Mainnet => "https://api.koios.rest/api/v1"
                  case CardanoNetwork.Preprod => "https://preprod.koios.rest/api/v1"
                  case CardanoNetwork.Preview => "https://preview.koios.rest/api/v1"
                  case CardanoNetwork.Testnet => "https://testnet.koios.rest/api/v1"
              }
            )

            CardanoConfig(
              backend = backend,
              network = network,
              blockfrost = BlockfrostConfig(
                apiUrl = blockfrostApiUrl,
                projectId = sys.env.getOrElse("BLOCKFROST_PROJECT_ID", "changeme")
              ),
              koios = KoiosConfig(
                apiUrl = koiosApiUrl
              ),
              ogmios = OgmiosConfig(
                url = sys.env.getOrElse("OGMIOS_URL", "http://localhost:1337")
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
                // Get API URL and ensure it ends with /
                val blockfrostUrl = cardanoConfig.getString("blockfrost.api-url")
                val normalizedUrl = if blockfrostUrl.endsWith("/") then blockfrostUrl else blockfrostUrl + "/"

                CardanoConfig(
                  backend = backend,
                  network = network,
                  blockfrost = BlockfrostConfig(
                    apiUrl = normalizedUrl,
                    projectId = cardanoConfig.getString("blockfrost.project-id")
                  ),
                  koios = KoiosConfig(
                    apiUrl = cardanoConfig.getString("koios.api-url")
                  ),
                  ogmios = OgmiosConfig(
                    url = cardanoConfig.getString("ogmios.url")
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
            case Right(cfg) if cfg.backend == CardanoBackend.Koios =>
                Right(cfg)
            case Right(cfg) if cfg.backend == CardanoBackend.Ogmios =>
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
        val apiUrl = network match {
            case CardanoNetwork.Mainnet => "https://cardano-mainnet.blockfrost.io/api/v0/"
            case CardanoNetwork.Preprod => "https://cardano-preprod.blockfrost.io/api/v0/"
            case CardanoNetwork.Preview => "https://cardano-preview.blockfrost.io/api/v0/"
            case CardanoNetwork.Testnet => "https://cardano-testnet.blockfrost.io/api/v0/"
        }

        CardanoConfig(
          backend = CardanoBackend.Blockfrost,
          network = network,
          blockfrost = BlockfrostConfig(apiUrl, projectId),
          koios = KoiosConfig(""),
          ogmios = OgmiosConfig("")
        )
    }

    /** Create Koios configuration for a specific network
      *
      * @param network
      *   Cardano network (mainnet, preprod, preview, testnet)
      */
    def koios(network: CardanoNetwork): CardanoConfig = {
        val apiUrl = network match {
            case CardanoNetwork.Mainnet => "https://api.koios.rest/api/v1"
            case CardanoNetwork.Preprod => "https://preprod.koios.rest/api/v1"
            case CardanoNetwork.Preview => "https://preview.koios.rest/api/v1"
            case CardanoNetwork.Testnet => "https://testnet.koios.rest/api/v1"
        }

        CardanoConfig(
          backend = CardanoBackend.Koios,
          network = network,
          blockfrost = BlockfrostConfig("", ""),
          koios = KoiosConfig(apiUrl),
          ogmios = OgmiosConfig("")
        )
    }

    /** Create Ogmios configuration for local node
      *
      * @param network
      *   Cardano network
      * @param url
      *   Ogmios WebSocket URL (default: http://localhost:1337)
      */
    def ogmios(
        network: CardanoNetwork,
        url: String = "http://localhost:1337"
    ): CardanoConfig = {
        CardanoConfig(
          backend = CardanoBackend.Ogmios,
          network = network,
          blockfrost = BlockfrostConfig("", ""),
          koios = KoiosConfig(""),
          ogmios = OgmiosConfig(url)
        )
    }
}
