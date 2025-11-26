package binocular

import com.bloxbean.cardano.client.address.Address
import com.bloxbean.cardano.client.plutus.spec.PlutusV3Script
import com.bloxbean.cardano.client.util.HexUtil
import com.typesafe.config.{Config, ConfigFactory}
import scala.util.{Failure, Success, Try}

/** Configuration for Binocular Oracle
  *
  * The oracle validator script address is automatically derived from the compiled BitcoinValidator
  * script hash for the specified network.
  *
  * Can be configured via:
  *   1. application.conf file
  *   2. Environment variables
  *   3. Direct construction
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
            case Success(_)  => Right(())
            case Failure(ex) => Left(s"Failed to derive oracle script address: ${ex.getMessage}")
        }
    }

    /** Get Address object for the oracle script */
    def getAddress(): Either[String, Address] = {
        Try {
            new Address(scriptAddress)
        } match {
            case Success(addr) => Right(addr)
            case Failure(ex)   => Left(s"Failed to parse oracle address: ${ex.getMessage}")
        }
    }

    override def toString: String = {
        s"OracleConfig(scriptAddress=$scriptAddress, network=$network, startHeight=$startHeight, maxHeadersPerTx=$maxHeadersPerTx)"
    }
}

object OracleConfig {

    /** Get the script CBOR hex, either from env/file or by compiling.
      * Priority: ORACLE_SCRIPT_CBOR env var > oracle-script.cbor file > compile
      */
    def getScriptCborHex(): String = {
        // First check environment variable
        sys.env.get("ORACLE_SCRIPT_CBOR").filter(_.nonEmpty).getOrElse {
            // Then check for cbor file
            val cborFile = new java.io.File("oracle-script.cbor")
            if (cborFile.exists()) {
                scala.io.Source.fromFile(cborFile).mkString.trim
            } else {
                // Fall back to compiling
                BitcoinContract.bitcoinProgram.doubleCborHex
            }
        }
    }

    /** Derive script address from compiled BitcoinValidator for given network.
      * Can be overridden via ORACLE_SCRIPT_ADDRESS env var.
      */
    def deriveScriptAddress(network: CardanoNetwork): String = {
        // First check if address is explicitly provided
        sys.env.get("ORACLE_SCRIPT_ADDRESS").filter(_.nonEmpty) match {
            case Some(addr) => addr
            case None =>
                // Get script CBOR (from env, file, or compile)
                val scriptCborHex = getScriptCborHex()

                // Create PlutusV3Script
                val plutusScript = PlutusV3Script
                    .builder()
                    .`type`("PlutusScriptV3")
                    .cborHex(scriptCborHex)
                    .build()
                    .asInstanceOf[PlutusV3Script]

                // Get CCL Network
                val cclNetwork = network match {
                    case CardanoNetwork.Mainnet =>
                        com.bloxbean.cardano.client.common.model.Networks.mainnet()
                    case CardanoNetwork.Preprod =>
                        com.bloxbean.cardano.client.common.model.Networks.preprod()
                    case CardanoNetwork.Preview =>
                        com.bloxbean.cardano.client.common.model.Networks.preview()
                    case CardanoNetwork.Testnet =>
                        com.bloxbean.cardano.client.common.model.Networks.testnet()
                }

                // Derive script address using AddressProvider
                val address = com.bloxbean.cardano.client.address.AddressProvider
                    .getEntAddress(plutusScript, cclNetwork)
                address.toBech32()
        }
    }

    /** Load from environment variables
      *
      * Expected environment variables:
      *   - ORACLE_START_HEIGHT: Optional starting block height
      *   - ORACLE_MAX_HEADERS_PER_TX: Maximum headers per transaction (default: 10)
      *   - ORACLE_POLL_INTERVAL: Poll interval in seconds (default: 60)
      *   - CARDANO_NETWORK: mainnet, preprod, preview, or testnet
      *
      * @param useDefaults
      *   If true, uses "mainnet" as default when CARDANO_NETWORK not set. If false, returns Left
      *   when CARDANO_NETWORK not set.
      */
    def fromEnv(useDefaults: Boolean = false): Either[String, OracleConfig] = {
        val networkStrOpt = sys.env.get("CARDANO_NETWORK")

        val networkStr = (networkStrOpt, useDefaults) match {
            case (Some(net), _) => net
            case (None, true)   => "mainnet"
            case (None, false)  => return Left("CARDANO_NETWORK environment variable not set")
        }

        val startHeight = sys.env.get("ORACLE_START_HEIGHT").flatMap(s => Try(s.toLong).toOption)
        val maxHeadersPerTx = sys.env
            .get("ORACLE_MAX_HEADERS_PER_TX")
            .flatMap(s => Try(s.toInt).toOption)
            .getOrElse(10)
        val pollInterval = sys.env
            .get("ORACLE_POLL_INTERVAL")
            .flatMap(s => Try(s.toInt).toOption)
            .getOrElse(60)

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
            val cardanoNetwork = if config.hasPath("binocular.cardano.network") then {
                config.getString("binocular.cardano.network")
            } else {
                "mainnet"
            }

            val startHeight = if oracleConfig.hasPath("start-height") then {
                Some(oracleConfig.getLong("start-height"))
            } else {
                None
            }
            val maxHeadersPerTx = if oracleConfig.hasPath("max-headers-per-tx") then {
                oracleConfig.getInt("max-headers-per-tx")
            } else {
                10
            }
            val pollInterval = if oracleConfig.hasPath("poll-interval") then {
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
            case Failure(ex) =>
                Left(s"Failed to load oracle config from application.conf: ${ex.getMessage}")
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
      * @param network
      *   Cardano network
      * @param startHeight
      *   Optional starting Bitcoin block height
      */
    def forNetwork(
        network: CardanoNetwork,
        startHeight: Option[Long] = None
    ): OracleConfig = {
        OracleConfig(network, startHeight)
    }
}
