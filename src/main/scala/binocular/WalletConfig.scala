package binocular

import pureconfig.*
import scalus.cardano.address.{Address, Network}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.hd.HdAccount
import scalus.crypto.ed25519.given

import scala.util.{Failure, Success, Try}

/** Configuration for Cardano wallet.
  *
  * Loaded automatically by PureConfig from reference.conf / application.conf / env vars.
  */
case class WalletConfig(
    mnemonic: String = ""
) derives ConfigReader {

    /** Create HdAccount from mnemonic phrase */
    def createHdAccount(): Either[String, HdAccount] = {
        Try {
            val mnemonicWords = mnemonic.trim.split("\\s+").toList

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

    /** Get base address (Bech32 format) */
    def getAddress(network: Network): Either[String, String] = {
        createHdAccount().flatMap { acc =>
            acc.baseAddress(network).toBech32.toEither.left.map(_.getMessage)
        }
    }

    /** Get base address as Scalus Address */
    def getBaseAddress(network: Network): Either[String, Address] = {
        createHdAccount().map(_.baseAddress(network))
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
        s"WalletConfig(mnemonic=$masked)"
    }
}
