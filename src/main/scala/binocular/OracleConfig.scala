package binocular

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.Credential
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import com.typesafe.config.{Config, ConfigFactory}
import scalus.uplc.builtin.ByteString

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
  * // From environment variables
  * OracleConfig.fromEnv()
  *
  * // From application.conf
  * OracleConfig.fromConfig()
  * }}}
  */
case class OracleConfig(
    network: CardanoNetwork,
    params: BitcoinValidatorParams,
    startHeight: Option[Long] = None,
    maxHeadersPerTx: Int = 10,
    pollInterval: Int = 60,
    transactionTimeout: Int = 120 // seconds - used for tx building, submission, UTxO polling
) {

    /** Get the script address derived from BitcoinValidator script hash */
    lazy val scriptAddress: String = OracleConfig.deriveScriptAddress(network, params)

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

    /** Derive script address from compiled BitcoinValidator for given network. */
    def deriveScriptAddress(network: CardanoNetwork, params: BitcoinValidatorParams): String = {
        val scriptHash = BitcoinContract.makeContract(params).script.scriptHash

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

    /** Parse a TxOutRef from string format "txhash#index" */
    private def parseTxOutRef(s: String): Either[String, TxOutRef] = {
        s.split("#") match {
            case Array(txHash, idx) =>
                Try {
                    TxOutRef(TxId(ByteString.fromHex(txHash)), BigInt(idx.toInt))
                } match {
                    case Success(ref) => Right(ref)
                    case Failure(ex)  => Left(s"Invalid TxOutRef format '$s': ${ex.getMessage}")
                }
            case _ => Left(s"Invalid TxOutRef format '$s'. Expected: <txhash>#<index>")
        }
    }

    /** Load from environment variables
      *
      * Expected environment variables:
      *   - ORACLE_TX_OUT_REF: TxOutRef for NFT minting (format: txhash#index)
      *   - ORACLE_OWNER_PKH: Hex-encoded 28-byte PubKeyHash of oracle owner
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

        val txOutRefStr = sys.env.get("ORACLE_TX_OUT_REF") match {
            case Some(s) if s.nonEmpty => s
            case _ => return Left("ORACLE_TX_OUT_REF environment variable not set")
        }

        val ownerPkhStr = sys.env.get("ORACLE_OWNER_PKH") match {
            case Some(s) if s.nonEmpty => s
            case _ => return Left("ORACLE_OWNER_PKH environment variable not set")
        }
        val ownerPkh = Try(PubKeyHash(ByteString.fromHex(ownerPkhStr))) match {
            case Success(pkh) => pkh
            case Failure(ex) =>
                return Left(s"Invalid ORACLE_OWNER_PKH: ${ex.getMessage}")
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
            txOutRef <- parseTxOutRef(txOutRefStr)
            params = BitcoinContract.validatorParams(txOutRef, ownerPkh)
            config = OracleConfig(
              network,
              params,
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
      *     tx-out-ref = "txhash#index"
      *     owner-pkh = "hex-encoded-28-byte-pubkeyhash"
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

            val txOutRefStr = if oracleConfig.hasPath("tx-out-ref") then {
                oracleConfig.getString("tx-out-ref")
            } else {
                return Left(
                  "binocular.oracle.tx-out-ref not set in config"
                )
            }

            val ownerPkhStr = if oracleConfig.hasPath("owner-pkh") then {
                oracleConfig.getString("owner-pkh")
            } else {
                return Left(
                  "binocular.oracle.owner-pkh not set in config"
                )
            }
            val ownerPkh = PubKeyHash(ByteString.fromHex(ownerPkhStr))

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
                txOutRef <- parseTxOutRef(txOutRefStr)
                params = BitcoinContract.validatorParams(txOutRef, ownerPkh)
                oracleConf = OracleConfig(
                  network,
                  params,
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
}
