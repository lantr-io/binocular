package binocular

import com.bloxbean.cardano.client.account.Account
import com.typesafe.config.{Config, ConfigFactory}
import scala.util.{Failure, Success, Try}

/** Configuration for Cardano wallet
  *
  * Supports mnemonic phrase-based wallet configuration. Can be configured via:
  *   1. application.conf file
  *   2. Environment variables
  *   3. Direct construction
  *
  * SECURITY WARNING: Storing mnemonic phrases in config files or environment variables is
  * convenient for development but NOT recommended for production. Consider using hardware wallets
  * or secure key management systems for mainnet.
  *
  * Examples:
  * {{{
  * // From 24-word mnemonic
  * val wallet = WalletConfig(
  *   mnemonic = "word1 word2 ... word24",
  *   network = CardanoNetwork.Preprod
  * )
  *
  * // Create account for transactions
  * val account = wallet.createAccount() match {
  *   case Right(acc) => acc
  *   case Left(error) => sys.error(s"Failed to create account: $error")
  * }
  *
  * // From environment variables
  * WalletConfig.fromEnv()
  *
  * // From application.conf
  * WalletConfig.fromConfig()
  * }}}
  */
case class WalletConfig(
    mnemonic: String,
    network: CardanoNetwork
) {

    /** Create Account from mnemonic phrase
      *
      * The Account object can be used to sign transactions.
      */
    def createAccount(): Either[String, Account] = {
        Try {
            val mnemonicWords = mnemonic.trim.split("\\s+").toList

            // Validate mnemonic length (12, 15, 18, 21, or 24 words)
            if !List(12, 15, 18, 21, 24).contains(mnemonicWords.length) then {
                throw new IllegalArgumentException(
                  s"Invalid mnemonic length: ${mnemonicWords.length} words. " +
                      "Expected 12, 15, 18, 21, or 24 words."
                )
            }

            // Create Account from mnemonic
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

            new Account(cclNetwork, mnemonic)
        } match {
            case Success(account) => Right(account)
            case Failure(ex) => Left(s"Failed to create account from mnemonic: ${ex.getMessage}")
        }
    }

    /** Get payment address (Bech32 format) */
    def getAddress(): Either[String, String] = {
        createAccount().map(_.baseAddress())
    }

    /** Validate configuration */
    def validate(): Either[String, Unit] = {
        if mnemonic.isEmpty || mnemonic == "changeme" then {
            Left(
              "Wallet mnemonic must be configured. Set WALLET_MNEMONIC or configure binocular.wallet.mnemonic"
            )
        } else {
            val words = mnemonic.trim.split("\\s+")
            if !List(12, 15, 18, 21, 24).contains(words.length) then {
                Left(s"Invalid mnemonic: expected 12, 15, 18, 21, or 24 words, got ${words.length}")
            } else {
                // Try to create account to validate mnemonic
                createAccount().map(_ => ())
            }
        }
    }

    /** Mask mnemonic for logging (show only first and last word) */
    override def toString: String = {
        val words = mnemonic.trim.split("\\s+")
        val masked = if words.length > 2 then {
            s"${words.head} ... ${words.last} (${words.length} words)"
        } else {
            "***"
        }
        s"WalletConfig(mnemonic=$masked, network=$network)"
    }
}

object WalletConfig {

    /** Load from environment variables
      *
      * Expected environment variables:
      *   - WALLET_MNEMONIC: 12/15/18/21/24-word mnemonic phrase
      *   - CARDANO_NETWORK: mainnet, preprod, preview, or testnet (defaults to preprod for safety)
      */
    def fromEnv(): Either[String, WalletConfig] = {
        val mnemonic = sys.env.getOrElse("WALLET_MNEMONIC", "")
        val networkStr = sys.env.getOrElse("CARDANO_NETWORK", "preprod")

        for {
            network <- CardanoNetwork.fromString(networkStr)
            config = WalletConfig(mnemonic, network)
            _ <- config.validate()
        } yield config
    }

    /** Load from Typesafe Config (application.conf)
      *
      * Expected config structure:
      * {{{
      * binocular {
      *   wallet {
      *     mnemonic = "word1 word2 ... word24"
      *     network = "preprod"  # or "mainnet", "preview", "testnet"
      *   }
      * }
      * }}}
      */
    def fromConfig(config: Config = ConfigFactory.load()): Either[String, WalletConfig] = {
        Try {
            val walletConfig = config.getConfig("binocular.wallet")

            val mnemonic = walletConfig.getString("mnemonic")

            // Use wallet.network if specified, otherwise fall back to cardano.network
            val networkStr = if walletConfig.hasPath("network") then {
                walletConfig.getString("network")
            } else if config.hasPath("binocular.cardano.network") then {
                config.getString("binocular.cardano.network")
            } else {
                "preprod"
            }

            for {
                network <- CardanoNetwork.fromString(networkStr)
                walletConf = WalletConfig(mnemonic, network)
                _ <- walletConf.validate()
            } yield walletConf
        } match {
            case Success(result) => result
            case Failure(ex) =>
                Left(s"Failed to load wallet config from application.conf: ${ex.getMessage}")
        }
    }

    /** Load configuration with priority: environment variables > application.conf
      *
      * Tries environment variables first, falls back to application.conf.
      */
    def load(): Either[String, WalletConfig] = {
        // Try environment variables first
        val envConfig = fromEnv()

        envConfig match {
            case Right(cfg) if cfg.mnemonic.nonEmpty && cfg.mnemonic != "changeme" =>
                Right(cfg)
            case _ =>
                // Fall back to config file
                fromConfig()
        }
    }

    /** Create wallet config from mnemonic phrase
      *
      * @param mnemonic
      *   12/15/18/21/24-word mnemonic phrase
      * @param network
      *   Cardano network (defaults to preprod for safety)
      */
    def fromMnemonic(
        mnemonic: String,
        network: CardanoNetwork = CardanoNetwork.Preprod
    ): Either[String, WalletConfig] = {
        val config = WalletConfig(mnemonic, network)
        config.validate().map(_ => config)
    }
}
