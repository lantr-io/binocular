package binocular

import scalus.cardano.address.{Address, Network}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.hd.HdAccount
import scalus.crypto.ed25519.given
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
  * // Create HD account for transactions
  * val account = wallet.createHdAccount() match {
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

    /** Create HdAccount from mnemonic phrase
      *
      * The HdAccount can be used to derive addresses and sign transactions.
      */
    def createHdAccount(): Either[String, HdAccount] = {
        Try {
            val mnemonicWords = mnemonic.trim.split("\\s+").toList

            // Validate mnemonic length (12, 15, 18, 21, or 24 words)
            if !List(12, 15, 18, 21, 24).contains(mnemonicWords.length) then {
                throw new IllegalArgumentException(
                  s"Invalid mnemonic length: ${mnemonicWords.length} words. " +
                      "Expected 12, 15, 18, 21, or 24 words."
                )
            }

            HdAccount.fromMnemonic(mnemonic.trim, "", accountIndex = 0)
        } match {
            case Success(account) => Right(account)
            case Failure(ex) => Left(s"Failed to create account from mnemonic: ${ex.getMessage}")
        }
    }

    /** Create TransactionSigner from mnemonic phrase */
    def createTransactionSigner(): Either[String, TransactionSigner] = {
        createHdAccount().map(acc => new TransactionSigner(Set(acc.paymentKeyPair)))
    }

    /** Get Scalus Network for this configuration */
    private def scalusNetwork: Network = network match {
        case CardanoNetwork.Mainnet => Network.Mainnet
        case CardanoNetwork.Preprod => Network.Testnet
        case CardanoNetwork.Preview => Network.Testnet
        case CardanoNetwork.Testnet => Network.Testnet
    }

    /** Get base address (Bech32 format) */
    def getAddress(): Either[String, String] = {
        createHdAccount().flatMap { acc =>
            acc.baseAddress(scalusNetwork).toBech32.toEither.left.map(_.getMessage)
        }
    }

    /** Get base address as Scalus Address */
    def getBaseAddress(): Either[String, Address] = {
        createHdAccount().map(_.baseAddress(scalusNetwork))
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
                createHdAccount().map(_ => ())
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
