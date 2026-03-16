package binocular

import pureconfig.*
import java.net.URI

/** Configuration for Bitcoin node connection.
  *
  * Loaded automatically by PureConfig from reference.conf / application.conf / env vars.
  */
case class BitcoinNodeConfig(
    url: String = "",
    username: String = "",
    password: String = "",
    network: String = "mainnet"
) derives ConfigReader {

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

    /** Parse network string to BitcoinNetwork enum */
    def bitcoinNetwork: BitcoinNetwork = network.toLowerCase match {
        case "mainnet" | "main" => BitcoinNetwork.Mainnet
        case "testnet" | "test" => BitcoinNetwork.Testnet
        case "regtest" | "reg"  => BitcoinNetwork.Regtest
        case _                  => BitcoinNetwork.Mainnet
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
