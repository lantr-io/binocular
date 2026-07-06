package binocular.cli.commands

import binocular.*
import binocular.bitcoin.Bip322
import binocular.cli.{Command, Console}
import binocular.watchtower.BifrostMessages

import org.bitcoins.core.crypto.ECPrivateKeyUtil
import scodec.bits.ByteVector

import java.nio.file.{Files, Paths}
import scala.util.boundary
import boundary.break

/** Sign the per-mint message digest that `pegin-complete --dry-run` prints, with a depositor WIF —
  * so the whole peg-in completion flow stays inside the toolchain (no external signer needed).
  *
  * The message is the 32-byte `sha2_256` digest of `"BFR-mint-v1" ‖ tm_txid ‖ peg_in_utxo_id ‖
  * serialiseData(recipient)` (printed as "Depositor signs (sha2_256 digest)" by `pegin-complete`).
  * This BIP340 Schnorr-signs that digest and prints the 64-byte signature to pass to
  * `pegin-complete --signature`, plus the x-only pubkey so you can confirm it matches the PIR
  * datum's `user_source_chain_pub_key` (verification fails otherwise).
  *
  * Pure crypto — needs no oracle/provider/config; `--key` is a path to a WIF file (e.g.
  * `heimdall/.keys/alice.wif`).
  */
case class SignPeginMsgCommand(keyPath: String, message: String) extends Command {

    override def execute(config: BinocularConfig): Int = boundary {
        Console.header("Sign Peg-In Message")

        val msgHex = message.trim.toLowerCase
        val isHex = msgHex.length % 2 == 0 && msgHex.forall(c => "0123456789abcdef".contains(c))
        if !isHex || msgHex.length != 64 then {
            Console.error(
              s"Invalid --message: expected 64 hex chars (32-byte digest), got '$message'"
            )
            break(1)
        }

        val wif =
            try Files.readString(Paths.get(keyPath)).trim
            catch {
                case e: Exception =>
                    Console.error(s"Reading WIF $keyPath: ${e.getMessage}"); break(1)
            }

        val priv =
            try ECPrivateKeyUtil.fromWIFToPrivateKey(wif).toPrivateKey
            catch { case e: Exception => Console.error(s"Parsing WIF: ${e.getMessage}"); break(1) }

        // BIP-322 (taproot key-path): the depositor signs the ASCII text "BFR-mint-v1:" ++ <digest>,
        // exactly what peg_in.ak reconstructs and verifies. Equivalent to a wallet's
        // signMessage(text, "bip322-simple"); done here from the WIF so the demo needs no browser.
        val text = BifrostMessages.mintTextPrefix + msgHex
        val (outputKey, sig) = Bip322.signKeypath(priv, ByteVector(text.getBytes("US-ASCII")))

        Console.info("signed text (BIP-322)", text)
        Console.info("taproot output key", outputKey.toHex)
        Console.info("  (must equal the PIR datum user_source_chain_pub_key / BFR beacon)", "")
        println()
        Console.info("signature (pass to --signature)", sig.toHex)
        0
    }
}
