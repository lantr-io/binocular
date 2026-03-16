package binocular

import pureconfig.*
import scalus.cardano.address.Network
import scalus.cardano.node.{BlockchainProvider, BlockfrostProvider}

import scala.concurrent.{ExecutionContext, Future}
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
    blockfrostProjectId: String = ""
) derives ConfigReader {

    /** Create BlockchainProvider based on configured backend type */
    def createBlockchainProvider()(using
        ec: ExecutionContext
    ): Either[String, BlockchainProvider] = {
        if blockfrostProjectId.isEmpty || blockfrostProjectId == "changeme" then {
            Left(
              "Blockfrost backend requires valid project ID. Set BLOCKFROST_PROJECT_ID or configure binocular.cardano.blockfrost-project-id"
            )
        } else {
            Try {
                val future: Future[BlockfrostProvider] = cardanoNetwork match {
                    case CardanoNetwork.Mainnet =>
                        BlockfrostProvider.mainnet(blockfrostProjectId)
                    case CardanoNetwork.Preprod =>
                        BlockfrostProvider.preprod(blockfrostProjectId)
                    case CardanoNetwork.Preview =>
                        BlockfrostProvider.preview(blockfrostProjectId)
                    case CardanoNetwork.Testnet =>
                        BlockfrostProvider.preview(blockfrostProjectId)
                }
                future.await(30.seconds)
            } match {
                case Success(provider) => Right(provider)
                case Failure(ex) =>
                    Left(s"Failed to create Blockfrost provider: ${ex.getMessage}")
            }
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
        if blockfrostProjectId.isEmpty || blockfrostProjectId == "changeme" then {
            Left("Blockfrost project ID must be configured")
        } else {
            Right(())
        }
    }

    override def toString: String = {
        val maskedId =
            if blockfrostProjectId.length > 8 then blockfrostProjectId.take(8) + "***"
            else "***"
        s"CardanoConfig(backend=$backend, network=$network, blockfrostProjectId=$maskedId)"
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
