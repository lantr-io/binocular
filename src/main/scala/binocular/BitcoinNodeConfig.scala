package binocular

import com.typesafe.config.{Config, ConfigFactory}
import java.net.URI

/** Configuration for Bitcoin node connection
  *
  * Supports both local and remote Bitcoin Core nodes. Can be configured via:
  *   1. application.conf file
  *   2. Environment variables
  *   3. Direct construction
  *
  * Examples:
  * {{{
  * // Local node
  * BitcoinNodeConfig(
  *   url = "http://localhost:8332",
  *   username = "bitcoin",
  *   password = "secret"
  * )
  *
  * // Remote node
  * BitcoinNodeConfig(
  *   url = "http://bitcoin.example.com:8332",
  *   username = "oracle",
  *   password = "secure_password"
  * )
  *
  * // From environment variables
  * BitcoinNodeConfig.fromEnv()
  *
  * // From application.conf
  * BitcoinNodeConfig.fromConfig()
  * }}}
  */
case class BitcoinNodeConfig(
    url: String,
    username: String,
    password: String,
    network: BitcoinNetwork = BitcoinNetwork.Mainnet
) {

    /** Get URI for Bitcoin node */
    def uri: URI = new URI(url)

    /** Validate configuration */
    def validate(): Either[String, Unit] = {
        if url.isEmpty then Left("Bitcoin node URL cannot be empty")
        // Allow empty username/password for API-key-in-URL services (QuickNode, GetBlock)
        else if !url.startsWith("http://") && !url.startsWith("https://") then
            Left(s"Bitcoin node URL must start with http:// or https://, got: $url")
        else Right(())
    }

    /** Check if this is a local node */
    def isLocal: Boolean = {
        url.contains("localhost") || url.contains("127.0.0.1") || url.contains("0.0.0.0")
    }

    /** Mask password for logging */
    override def toString: String = {
        s"BitcoinNodeConfig(url=$url, username=$username, password=***, network=$network, isLocal=$isLocal)"
    }
}

/** Bitcoin network selection */
enum BitcoinNetwork {
    case Mainnet
    case Testnet
    case Regtest
}

object BitcoinNodeConfig {

    /** Load from environment variables
      *
      * Expected environment variables:
      *   - BITCOIN_NODE_URL (or bitcoind_rpc_url)
      *   - BITCOIN_NODE_USER (or bitcoind_rpc_user)
      *   - BITCOIN_NODE_PASSWORD (or bitcoind_rpc_password)
      *   - BITCOIN_NETWORK (optional, defaults to mainnet)
      */
    def fromEnv(): Either[String, BitcoinNodeConfig] = {
        val url = Option(System.getenv("BITCOIN_NODE_URL"))
            .orElse(Option(System.getenv("bitcoind_rpc_url")))
            .getOrElse("")

        val username = Option(System.getenv("BITCOIN_NODE_USER"))
            .orElse(Option(System.getenv("bitcoind_rpc_user")))
            .getOrElse("")

        val password = Option(System.getenv("BITCOIN_NODE_PASSWORD"))
            .orElse(Option(System.getenv("bitcoind_rpc_password")))
            .getOrElse("")

        val network = Option(System.getenv("BITCOIN_NETWORK"))
            .flatMap(parseNetwork)
            .getOrElse(BitcoinNetwork.Mainnet)

        val config = BitcoinNodeConfig(url, username, password, network)
        config.validate().map(_ => config)
    }

    /** Load from Typesafe Config (application.conf)
      *
      * Expected config structure:
      * {{{
      * binocular {
      *   bitcoin-node {
      *     url = "http://localhost:8332"
      *     username = "bitcoin"
      *     password = "secret"
      *     network = "mainnet"  # or "testnet", "regtest"
      *   }
      * }
      * }}}
      */
    def fromConfig(config: Config = ConfigFactory.load()): Either[String, BitcoinNodeConfig] = {
        try {
            val bitcoinConfig = config.getConfig("binocular.bitcoin-node")

            val url = bitcoinConfig.getString("url")
            val username = bitcoinConfig.getString("username")
            val password = bitcoinConfig.getString("password")
            val network =
                if bitcoinConfig.hasPath("network") then
                    parseNetwork(bitcoinConfig.getString("network"))
                        .getOrElse(BitcoinNetwork.Mainnet)
                else BitcoinNetwork.Mainnet

            val nodeConfig = BitcoinNodeConfig(url, username, password, network)
            nodeConfig.validate().map(_ => nodeConfig)
        } catch {
            case e: Exception =>
                Left(s"Failed to load Bitcoin node config: ${e.getMessage}")
        }
    }

    /** Load from config with fallback to environment variables
      *
      *   1. Try environment variables first (highest priority)
      *   2. Fall back to application.conf
      *   3. Return error if neither works
      */
    def load(): Either[String, BitcoinNodeConfig] = {
        fromEnv().orElse(fromConfig())
    }

    /** Create config for local Bitcoin node with defaults */
    def local(
        port: Int = 8332,
        username: String = "bitcoin",
        password: String
    ): BitcoinNodeConfig = {
        BitcoinNodeConfig(
          url = s"http://localhost:$port",
          username = username,
          password = password,
          network = BitcoinNetwork.Mainnet
        )
    }

    /** Create config for remote Bitcoin node */
    def remote(
        host: String,
        port: Int = 8332,
        username: String,
        password: String,
        useSsl: Boolean = false
    ): BitcoinNodeConfig = {
        val protocol = if useSsl then "https" else "http"
        BitcoinNodeConfig(
          url = s"$protocol://$host:$port",
          username = username,
          password = password,
          network = BitcoinNetwork.Mainnet
        )
    }

    private def parseNetwork(s: String): Option[BitcoinNetwork] = {
        s.toLowerCase match {
            case "mainnet" | "main" => Some(BitcoinNetwork.Mainnet)
            case "testnet" | "test" => Some(BitcoinNetwork.Testnet)
            case "regtest" | "reg"  => Some(BitcoinNetwork.Regtest)
            case _                  => None
        }
    }
}
