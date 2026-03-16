package binocular

import pureconfig.*

/** Top-level configuration — single entry point.
  *
  * Loaded from reference.conf + application.conf + optional --config file + env vars via
  * PureConfig.
  */
case class BinocularConfig(
    bitcoinNode: BitcoinNodeConfig,
    cardano: CardanoConfig,
    wallet: WalletConfig,
    oracle: OracleConfig
) derives ConfigReader

object BinocularConfig {

    /** Load configuration with optional override file.
      *
      * Priority (highest wins): env vars > --config file > application.conf > reference.conf
      */
    def load(configPath: Option[String] = None): BinocularConfig = {
        val source = configPath match {
            case Some(path) =>
                ConfigSource.file(path).withFallback(ConfigSource.default)
            case None =>
                ConfigSource.default
        }
        source.at("binocular").loadOrThrow[BinocularConfig]
    }
}
