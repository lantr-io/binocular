package binocular

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.plutus.spec.PlutusV3Script
import com.bloxbean.cardano.client.util.HexUtil
import com.typesafe.config.{Config, ConfigFactory}
import scala.util.{Try, Success, Failure}

/** Configuration for Binocular Oracle
  *
  * The oracle validator script address is automatically derived from the
  * compiled BitcoinValidator script hash for the specified network.
  *
  * Can be configured via:
  * 1. application.conf file
  * 2. Environment variables
  * 3. Direct construction
  *
  * Examples:
  * {{{
  * // Mainnet oracle (address derived automatically)
  * val config = OracleConfig.forNetwork(CardanoNetwork.Mainnet)
  *
  * // From environment variables
  * OracleConfig.fromEnv()
  *
  * // From application.conf
  * OracleConfig.fromConfig()
  * }}}
  */
case class OracleConfig(
    network: CardanoNetwork,
    startHeight: Option[Long] = None,
    maxHeadersPerTx: Int = 10,
    pollInterval: Int = 60
) {

  /** Get the script address derived from BitcoinValidator script hash */
  lazy val scriptAddress: String = OracleConfig.deriveScriptAddress(network)

  /** Validate configuration */
  def validate(): Either[String, Unit] = {
    // Validate that script address can be derived
    Try {
      new Address(scriptAddress)
    } match {
      case Success(_) => Right(())
      case Failure(ex) => Left(s"Failed to derive oracle script address: ${ex.getMessage}")
    }
  }

  /** Get Address object for the oracle script */
  def getAddress(): Either[String, Address] = {
    Try {
      new Address(scriptAddress)
    } match {
      case Success(addr) => Right(addr)
      case Failure(ex) => Left(s"Failed to parse oracle address: ${ex.getMessage}")
    }
  }

  override def toString: String = {
    s"OracleConfig(scriptAddress=$scriptAddress, network=$network, startHeight=$startHeight, maxHeadersPerTx=$maxHeadersPerTx)"
  }
}

object OracleConfig {

  /** Derive script address from compiled BitcoinValidator for given network */
  def deriveScriptAddress(network: CardanoNetwork): String = {
    // Get compiled script
    val program = BitcoinContract.bitcoinProgram
    val scriptCborHex = program.doubleCborHex

    // Create PlutusV3Script
    val plutusScript = PlutusV3Script.builder()
      .`type`("PlutusScriptV3")
      .cborHex(scriptCborHex)
      .build()
      .asInstanceOf[PlutusV3Script]

    // Get CCL Network
    val cclNetwork = network match {
      case CardanoNetwork.Mainnet => com.bloxbean.cardano.client.common.model.Networks.mainnet()
      case CardanoNetwork.Preprod => com.bloxbean.cardano.client.common.model.Networks.preprod()
      case CardanoNetwork.Preview => com.bloxbean.cardano.client.common.model.Networks.preview()
      case CardanoNetwork.Testnet => com.bloxbean.cardano.client.common.model.Networks.testnet()
    }

    // Derive script address using AddressProvider
    val address = com.bloxbean.cardano.client.address.AddressProvider.getEntAddress(plutusScript, cclNetwork)
    address.toBech32()
  }

  /** Load from environment variables
    *
    * Expected environment variables:
    * - ORACLE_START_HEIGHT: Optional starting block height
    * - ORACLE_MAX_HEADERS_PER_TX: Maximum headers per transaction (default: 10)
    * - ORACLE_POLL_INTERVAL: Poll interval in seconds (default: 60)
    * - CARDANO_NETWORK: mainnet, preprod, preview, or testnet (default: mainnet)
    */
  def fromEnv(): Either[String, OracleConfig] = {
    val startHeight = sys.env.get("ORACLE_START_HEIGHT").flatMap(s => Try(s.toLong).toOption)
    val maxHeadersPerTx = sys.env.get("ORACLE_MAX_HEADERS_PER_TX")
      .flatMap(s => Try(s.toInt).toOption)
      .getOrElse(10)
    val pollInterval = sys.env.get("ORACLE_POLL_INTERVAL")
      .flatMap(s => Try(s.toInt).toOption)
      .getOrElse(60)
    val networkStr = sys.env.getOrElse("CARDANO_NETWORK", "mainnet")

    for {
      network <- CardanoNetwork.fromString(networkStr)
      config = OracleConfig(network, startHeight, maxHeadersPerTx, pollInterval)
      _ <- config.validate()
    } yield config
  }

  /** Load from Typesafe Config (application.conf)
    *
    * Expected config structure:
    * {{{
    * binocular {
    *   oracle {
    *     start-height = 800000  # optional
    *     max-headers-per-tx = 10
    *     poll-interval = 60
    *   }
    *   cardano {
    *     network = "mainnet"
    *   }
    * }
    * }}}
    */
  def fromConfig(config: Config = ConfigFactory.load()): Either[String, OracleConfig] = {
    Try {
      val oracleConfig = config.getConfig("binocular.oracle")
      val cardanoNetwork = if (config.hasPath("binocular.cardano.network")) {
        config.getString("binocular.cardano.network")
      } else {
        "mainnet"
      }

      val startHeight = if (oracleConfig.hasPath("start-height")) {
        Some(oracleConfig.getLong("start-height"))
      } else {
        None
      }
      val maxHeadersPerTx = if (oracleConfig.hasPath("max-headers-per-tx")) {
        oracleConfig.getInt("max-headers-per-tx")
      } else {
        10
      }
      val pollInterval = if (oracleConfig.hasPath("poll-interval")) {
        oracleConfig.getInt("poll-interval")
      } else {
        60
      }

      for {
        network <- CardanoNetwork.fromString(cardanoNetwork)
        oracleConf = OracleConfig(network, startHeight, maxHeadersPerTx, pollInterval)
        _ <- oracleConf.validate()
      } yield oracleConf
    } match {
      case Success(result) => result
      case Failure(ex) => Left(s"Failed to load oracle config from application.conf: ${ex.getMessage}")
    }
  }

  /** Load configuration with priority: environment variables > application.conf */
  def load(): Either[String, OracleConfig] = {
    fromEnv().orElse(fromConfig())
  }

  /** Create oracle config for a specific network
    *
    * Script address is automatically derived from the BitcoinValidator.
    *
    * @param network Cardano network
    * @param startHeight Optional starting Bitcoin block height
    */
  def forNetwork(
      network: CardanoNetwork,
      startHeight: Option[Long] = None
  ): OracleConfig = {
    OracleConfig(network, startHeight)
  }
}
