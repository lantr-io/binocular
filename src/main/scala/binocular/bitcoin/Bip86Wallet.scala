package binocular.bitcoin

import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}
import org.bitcoins.core.crypto.{BIP39Seed, ExtKeyVersion, MnemonicCode}
import org.bitcoins.core.hd.{HDChainType, HDCoinType, TaprootHDPath}
import org.bitcoins.core.protocol.Bech32mAddress
import org.bitcoins.core.protocol.script.TaprootScriptPubKey
import org.bitcoins.crypto.{ECPrivateKey, FieldElement, Sign}
import scodec.bits.ByteVector

import scala.util.control.NonFatal

/** Two fixed BIP86 roles, derived in memory. No key export, address allocation or network I/O.
  * Identity is /0/0; funding, change and payouts share /0/1. The demo CLI rejects mainnet.
  */
final class Bip86Wallet private (val identity: Bip86Key, val funding: Bip86Key)

object Bip86Wallet {
    def fromMnemonic(mnemonic: String, network: BitcoinNetwork): Either[String, Bip86Wallet] =
        try {
            val words = MnemonicCode.fromWords(mnemonic.trim.split("\\s+").toVector)
            val mainnet = network == BitcoinNetwork.Mainnet
            val coin = if mainnet then HDCoinType.Bitcoin else HDCoinType.Testnet
            val version =
                if mainnet then ExtKeyVersion.LegacyMainNetPriv
                else ExtKeyVersion.LegacyTestNet3Priv
            // Testnet4 shares testnet's "tb" prefix, so it encodes as TestNet3.
            val params: NetworkParameters = network match {
                case BitcoinNetwork.Mainnet => MainNet
                case BitcoinNetwork.Regtest => RegTest
                case _                      => TestNet3
            }
            // Same empty BIP39 passphrase as the existing Cardano wallet.
            val master = BIP39Seed.fromMnemonic(words).toExtPrivateKey(version)
            def derive(index: Int): Bip86Key = {
                val path = TaprootHDPath(coin, 0, HDChainType.External, index)
                new Bip86Key(master.deriveChildPrivKey(path).key, path.toString, params)
            }
            Right(new Bip86Wallet(derive(0), derive(1)))
        } catch {
            // Library validation errors may contain mnemonic words or key bytes. Never forward them.
            case NonFatal(_) =>
                Left("Cannot derive Bitcoin wallet: invalid mnemonic or key derivation failed")
        }
}

/** One BIP86 role: its key-path output and the signer for it. Not a case class: neither generated
  * accessors nor toString expose the private key.
  */
final class Bip86Key private[bitcoin] (
    privateKey: ECPrivateKey,
    val path: String,
    network: NetworkParameters
) {
    val internalKey: ByteVector = privateKey.toXOnly.bytes
    val taprootScriptPubKey: TaprootScriptPubKey =
        TaprootScriptPubKey.fromInternalKey(privateKey.toXOnly)
    val scriptPubKey: ByteVector = taprootScriptPubKey.asmBytes
    val outputKey: ByteVector = taprootScriptPubKey.pubKey.bytes
    val address: String = Bech32mAddress(taprootScriptPubKey, network).value

    // BIP341 taproot_tweak_seckey: normalize to even Y BEFORE adding TapTweak(P).
    // bitcoin-s owns parity normalization, tagged hashing and scalar arithmetic (including zero).
    // https://github.com/bitcoin/bips/blob/master/bip-0341.mediawiki#constructing-and-spending-taproot-outputs
    private val signingKey = privateKey.schnorrKey.fieldElement
        .add(FieldElement(privateKey.toXOnly.computeTapTweakHash(None).bytes))
        .toPrivateKey

    /** Signs key-path spends of this role's output. bitcoin-s expects the TWEAKED key here and
      * refuses a signer whose public key is not the output key.
      */
    val signer: Sign = signingKey

    /** A BIP-340 signature over a 32-byte digest by this role's TWEAKED key, the output key Q.
      *
      * Every signature the protocol asks of this role verifies under `outputKey`: the peg-in refund
      * leaf names `Q_auth`, and so does BIP-322. Signing with the raw HD child instead yields a
      * well-formed signature that verifies nowhere.
      */
    def signDigest(digest: ByteVector): ByteVector = signingKey.schnorrSign(digest).bytes

    /** BIP322 authorization verifies under Q_auth = outputKey, not the raw HD child. */
    def signMessage(message: ByteVector): ByteVector =
        signDigest(Bip322.keypathSighash(message, outputKey))

    override def toString: String = s"Bip86Key($path, $address)"
}
