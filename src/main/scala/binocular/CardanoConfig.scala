package binocular

import pureconfig.*
import scalus.cardano.address.Network
import scalus.cardano.ledger.SlotConfig
import scalus.cardano.node.{BlockchainProvider, BlockfrostProvider}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}
import scalus.utils.await

/** Configuration for Cardano network connection.
  *
  * Loaded automatically by PureConfig from reference.conf / application.conf / env vars.
  */
case class CardanoConfig(
    network: String = "mainnet",
    backend: String = "blockfrost",
    blockfrostProjectId: String = "",
    yaciStoreUrl: String = "http://localhost:8080/api/v1",
    yaciAdminUrl: String = "http://localhost:10000/local-cluster/api"
) derives ConfigReader {

    /** Create BlockchainProvider based on configured backend type */
    def createBlockchainProvider()(using
        ec: ExecutionContext
    ): Either[String, BlockchainProvider] = {
        backend.toLowerCase match {
            case "yaci" =>
                Try {
                    BlockfrostProvider
                        .localYaci(yaciStoreUrl, yaciAdminUrl)
                        .await(30.seconds)
                } match {
                    case Success(provider) => Right(provider)
                    case Failure(ex) =>
                        Left(s"Failed to create Yaci provider: ${ex.getMessage}")
                }
            case "blockfrost" =>
                if blockfrostProjectId.isEmpty || blockfrostProjectId == "changeme" then {
                    Left(
                      "Blockfrost backend requires valid project ID. Set BLOCKFROST_PROJECT_ID or configure binocular.cardano.blockfrost-project-id"
                    )
                } else {
                    Try {
                        val (baseUrl, network, defaultSlotConfig) = cardanoNetwork match {
                            case CardanoNetwork.Mainnet =>
                                (
                                  BlockfrostProvider.mainnetUrl,
                                  Network.Mainnet,
                                  SlotConfig.mainnet
                                )
                            case CardanoNetwork.Preprod =>
                                (
                                  BlockfrostProvider.preprodUrl,
                                  Network.Testnet,
                                  SlotConfig.preprod
                                )
                            case CardanoNetwork.Preview =>
                                (
                                  BlockfrostProvider.previewUrl,
                                  Network.Testnet,
                                  SlotConfig.preview
                                )
                            case CardanoNetwork.Testnet =>
                                (
                                  BlockfrostProvider.previewUrl,
                                  Network.Testnet,
                                  SlotConfig.preview
                                )
                        }
                        val slotConfig = CardanoConfig
                            .fetchCalibratedSlotConfig(
                              blockfrostProjectId,
                              baseUrl,
                              defaultSlotConfig
                            )
                            .getOrElse(defaultSlotConfig)
                        BlockfrostProvider
                            .create(
                              blockfrostProjectId,
                              baseUrl,
                              network,
                              slotConfig
                            )
                            .await(30.seconds)
                    } match {
                        case Success(provider) => Right(provider)
                        case Failure(ex) =>
                            Left(s"Failed to create Blockfrost provider: ${ex.getMessage}")
                    }
                }
            case other =>
                Left(
                  s"Unknown Cardano backend: $other. Valid options: blockfrost, yaci"
                )
        }
    }

    /** Parse network string to CardanoNetwork enum */
    def cardanoNetwork: CardanoNetwork =
        CardanoNetwork.fromString(network).getOrElse(CardanoNetwork.Mainnet)

    /** Get Scalus Network for this configuration */
    def scalusNetwork: Network = cardanoNetwork match {
        case CardanoNetwork.Mainnet => Network.Mainnet
        case CardanoNetwork.Preprod => Network.Testnet
        case CardanoNetwork.Preview => Network.Testnet
        case CardanoNetwork.Testnet => Network.Testnet
    }

    /** Validate configuration */
    def validate(): Either[String, Unit] = {
        backend.toLowerCase match {
            case "yaci" => Right(())
            case "blockfrost" =>
                if blockfrostProjectId.isEmpty || blockfrostProjectId == "changeme" then {
                    Left("Blockfrost project ID must be configured")
                } else {
                    Right(())
                }
            case other =>
                Left(s"Unknown Cardano backend: $other. Valid options: blockfrost, yaci")
        }
    }

    override def toString: String = {
        val maskedId =
            if blockfrostProjectId.length > 8 then blockfrostProjectId.take(8) + "***"
            else "***"
        s"CardanoConfig(backend=$backend, network=$network, blockfrostProjectId=$maskedId)"
    }
}

object CardanoConfig {

    /** Fetch the latest block from Blockfrost and calibrate SlotConfig from actual network data.
      *
      * Testnets can accumulate slot drift from outages/restarts, causing the theoretical SlotConfig
      * to compute slots ahead of reality. This queries `/blocks/latest` to get the actual tip slot
      * and time, then adjusts zeroTime accordingly.
      */
    def fetchCalibratedSlotConfig(
        apiKey: String,
        baseUrl: String,
        defaultSlotConfig: SlotConfig
    ): Option[SlotConfig] = {
        Try {
            val client = java.net.http.HttpClient.newHttpClient()
            val request = java.net.http.HttpRequest
                .newBuilder()
                .uri(java.net.URI.create(s"$baseUrl/blocks/latest"))
                .header("project_id", apiKey)
                .GET()
                .build()
            val response =
                client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString())
            if response.statusCode() == 200 then {
                val json = ujson.read(response.body())
                val tipSlot = json("slot").num.toLong
                val tipTime = json("time").num.toLong // unix seconds
                // Calibrate: zeroTime = tipTimeMs - (tipSlot - zeroSlot) * slotLength
                val calibratedZeroTime =
                    tipTime * 1000L - (tipSlot - defaultSlotConfig.zeroSlot) * defaultSlotConfig.slotLength
                Some(
                  SlotConfig(
                    calibratedZeroTime,
                    defaultSlotConfig.zeroSlot,
                    defaultSlotConfig.slotLength
                  )
                )
            } else None
        }.getOrElse(None)
    }
}

/** Cardano backend types */
enum CardanoBackend {
    case Blockfrost
    case Yaci
}

object CardanoBackend {
    def fromString(s: String): Either[String, CardanoBackend] = s.toLowerCase match {
        case "blockfrost" => Right(Blockfrost)
        case "yaci"       => Right(Yaci)
        case _            => Left(s"Unknown Cardano backend: $s. Valid options: blockfrost, yaci")
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
