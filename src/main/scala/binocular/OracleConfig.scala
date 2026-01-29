package binocular

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{Credential, Script, ScriptHash}
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
    pollInterval: Int = 60,
    transactionTimeout: Int = 120 // seconds - used for tx building, submission, UTxO polling
) {

    /** Get the script address derived from BitcoinValidator script hash */
    lazy val scriptAddress: String = OracleConfig.deriveScriptAddress(network)

    /** Validate configuration */
    def validate(): Either[String, Unit] = {
        Try {
            Address.fromBech32(scriptAddress)
        } match {
            case Success(_)  => Right(())
            case Failure(ex) => Left(s"Failed to derive oracle script address: ${ex.getMessage}")
        }
    }

    /** Get Address object for the oracle script */
    def getAddress(): Either[String, Address] = {
        Try {
            Address.fromBech32(scriptAddress)
        } match {
            case Success(addr) => Right(addr)
            case Failure(ex)   => Left(s"Failed to parse oracle address: ${ex.getMessage}")
        }
    }

    override def toString: String = {
        s"OracleConfig(scriptAddress=$scriptAddress, network=$network, startHeight=$startHeight, maxHeadersPerTx=$maxHeadersPerTx, transactionTimeout=$transactionTimeout)"
    }
}

object OracleConfig {

    /** Get the script CBOR hex from compiled script. */
    def getScriptCborHex(): String = {
        BitcoinContract.bitcoinProgram.doubleCborHex
    }

    /** Get the PlutusScript from the compiled BitcoinValidator */
    def getPlutusScript(): Script.PlutusV3 = {
        Script.PlutusV3(BitcoinContract.bitcoinProgram.cborByteString)
    }

    /** Get the script hash */
    def getScriptHash(): ScriptHash = {
        getPlutusScript().scriptHash
    }

    /** Derive script address from compiled BitcoinValidator for given network. */
    def deriveScriptAddress(network: CardanoNetwork): String = {
        val scriptHash = getScriptHash()

        val scalusNetwork = network match {
            case CardanoNetwork.Mainnet => Network.Mainnet
            case CardanoNetwork.Preprod => Network.Testnet
            case CardanoNetwork.Preview => Network.Testnet
            case CardanoNetwork.Testnet => Network.Testnet
        }

        // Create enterprise address with script credential
        val address = Address(scalusNetwork, Credential.ScriptHash(scriptHash))
        address.asInstanceOf[scalus.cardano.address.ShelleyAddress].toBech32.get
    }

    /** Load from environment variables
      *
      * Expected environment variables:
      *   - ORACLE_START_HEIGHT: Optional starting block height
      *   - ORACLE_MAX_HEADERS_PER_TX: Maximum headers per transaction (default: 10)
      *   - ORACLE_POLL_INTERVAL: Poll interval in seconds (default: 60)
      *   - ORACLE_TRANSACTION_TIMEOUT: Transaction timeout in seconds (default: 120)
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
        val transactionTimeout = sys.env
            .get("ORACLE_TRANSACTION_TIMEOUT")
            .flatMap(s => Try(s.toInt).toOption)
            .getOrElse(120)

        for {
            network <- CardanoNetwork.fromString(networkStr)
            config = OracleConfig(
              network,
              startHeight,
              maxHeadersPerTx,
              pollInterval,
              transactionTimeout
            )
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
      *     transaction-timeout = 120
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
            val transactionTimeout = if oracleConfig.hasPath("transaction-timeout") then {
                oracleConfig.getInt("transaction-timeout")
            } else {
                120
            }

            for {
                network <- CardanoNetwork.fromString(cardanoNetwork)
                oracleConf = OracleConfig(
                  network,
                  startHeight,
                  maxHeadersPerTx,
                  pollInterval,
                  transactionTimeout
                )
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
