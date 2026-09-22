package binocular.bitcoin

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{EmptyTransaction, Transaction, TransactionOutput}
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.core.wallet.builder.{RawTxSigner, StandardNonInteractiveFinalizer}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.TaprootKeyPathInputInfo
import org.bitcoins.crypto.HashType
import scodec.bits.ByteVector

import scala.util.Try

/** The peg-in DEPOSIT transaction (spec §10): the peg-in P2TR at vout 0, the `BFR` beacon at vout
  * 1, change last. bitcoin-s sizes, funds and signs it.
  */
object PegInDeposit {

    /** `OP_RETURN` then a 35-byte push of the ASCII tag `BFR`; the 32-byte `q_auth` follows, which
      * makes the beacon scriptPubKey 37 bytes.
      */
    val BeaconPrefix: ByteVector = ByteVector.fromValidHex("6a23424652")

    /** The P2TR paying the quorum. `PegInProofBundle` parses the deposit at this index. */
    val DepositVout: Int = 0

    /** Build and sign the deposit, or say why it cannot be built.
      *
      * The layout is fixed: `bifrost/bitcoin.ak` reads the beacon with
      * `get_vout_scriptpubkey(raw_tx, 1)`, so vout 0 and 1 are consensus for the mint, and the
      * finalizer appends change after them. The deposit output keeps `amountSat` whatever the fee
      * is, because `[CPI-6]` mints fBTC equal to the value parsed from that output.
      *
      * Every coin offered is spent. ponytail: a demo wallet holds a handful of outputs; add
      * `CoinSelector.accumulateLargest` if it ever fragments enough for the fee to matter.
      */
    def build(
        tree: PeginTreeParams,
        qAuth: ByteVector,
        amountSat: Long,
        coins: Seq[ScannedBitcoinOutput],
        funding: Bip86Key,
        feeRateSatPerKvb: Long
    ): Either[String, Transaction] =
        for
            peginTree <- Taproot.peginTree(tree, qAuth)
            _ <- Either.cond(coins.nonEmpty, (), "no funding coins")
            _ <- Either.cond(
              coins.forall(_.scriptPubKey == funding.scriptPubKey),
              (),
              "a funding output does not pay the funding address"
            )
            _ <- Either.cond(feeRateSatPerKvb >= 0, (), s"negative fee rate $feeRateSatPerKvb")
            tx <- Try {
                val spk = funding.taprootScriptPubKey
                val prevouts = PreviousOutputMap(coins.map { c =>
                    c.outpoint -> TransactionOutput(Satoshis(c.amountSat), spk)
                }.toMap)
                // bitcoin-s expects the tweaked key and does not tweak it itself.
                val inputs = coins.map { c =>
                    TaprootKeyPathInputInfo(c.outpoint, Satoshis(c.amountSat), spk, prevouts)
                        .toSpendingInfo(
                          EmptyTransaction,
                          Vector(funding.signer),
                          HashType.sigHashDefault
                        )
                }.toVector
                val outputs = Vector(
                  TransactionOutput(Satoshis(amountSat), peginTree.scriptPubKey),
                  TransactionOutput(
                    Satoshis.zero,
                    ScriptPubKey.fromAsmBytes(BeaconPrefix ++ qAuth)
                  )
                )
                // Whole sat/vB, rounded up: never underpays the quoted sat/kvB rate.
                val rate = SatoshisPerVirtualByte(Satoshis((feeRateSatPerKvb + 999) / 1000))
                val unsigned = StandardNonInteractiveFinalizer.txFrom(outputs, inputs, rate, spk)
                RawTxSigner.sign(unsigned, inputs)
            }.toEither.left.map(e => s"deposit build failed: $e")
        yield tx
}
