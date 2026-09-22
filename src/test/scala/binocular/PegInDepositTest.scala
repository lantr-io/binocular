package binocular

import binocular.bitcoin.*

import org.bitcoins.core.crypto.{TaprootSerializationOptions, TaprootTxSigComponent, TransactionSignatureSerializer}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{TransactionOutPoint, TransactionOutput, WitnessTransaction}
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.crypto.{DoubleSha256Digest, SIGHASH_DEFAULT, SchnorrDigitalSignature, SchnorrPublicKey}
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector

/** The peg-in DEPOSIT: the output layout `bifrost/bitcoin.ak` reads, the fee, and the signatures,
  * with bitcoin-s doing the sizing, change and signing. The tree is `tuple_a_parity_zero` of
  * `taproot-pegin-vectors.json`.
  */
class PegInDepositTest extends AnyFunSuite {
    private val a = TaprootVectors.tupleA
    private val tree = PeginTreeParams(
      TaprootVectors.hex(a("y51")),
      TaprootVectors.hex(a("y_federation")),
      a("federation_csv_blocks").num.toInt,
      a("pegin_refund_timeout_blocks").num.toInt
    )
    private val qAuth = TaprootVectors.hex(a("q_auth"))
    private val wallet =
        Bip86Wallet.fromMnemonic("abandon " * 11 + "about", BitcoinNetwork.Testnet4).toOption.get
    private val funding = wallet.funding

    private def coin(fill: Int, vout: Int, sat: Long) = ScannedBitcoinOutput(
      TransactionOutPoint(DoubleSha256Digest(ByteVector.fill(32)(fill.toByte)), UInt32(vout)),
      sat,
      funding.scriptPubKey,
      90,
      false
    )
    private def build(
        coins: Seq[ScannedBitcoinOutput],
        amountSat: Long = 30000,
        rate: Long = 1000
    ) =
        PegInDeposit.build(tree, qAuth, amountSat, coins, funding, rate)
    private def built(coins: Seq[ScannedBitcoinOutput], rate: Long = 1000): WitnessTransaction =
        build(coins, rate = rate).fold(fail(_), identity).asInstanceOf[WitnessTransaction]

    test("vout 0 is the peg-in amount exactly, vout 1 the 37-byte BFR beacon, change last") {
        val tx = built(Seq(coin(0x22, 1, 100000)))
        assert(tx.outputs.length == 3)
        assert(
          tx.outputs(0) == TransactionOutput(
            Satoshis(30000),
            ScriptPubKey.fromAsmBytes(TaprootVectors.hex(a("script_pub_key")))
          )
        )
        assert(tx.outputs(1).value == Satoshis.zero)
        assert(
          tx.outputs(1).scriptPubKey.asmBytes == ByteVector.fromValidHex("6a23424652") ++ qAuth
        )
        assert(tx.outputs(1).scriptPubKey.asmBytes.length == 37)
        assert(tx.outputs(2).scriptPubKey.asmBytes == funding.scriptPubKey)
        assert(tx.version.toInt == 2 && tx.lockTime.toLong == 0)
    }

    test(
      "the fee never underpays the rate rounded up to whole sat/vB, and over-pays at most 1 vB"
    ) {
        // bitcoin-s sizes the fee on a dummy witness one byte larger than a SIGHASH_DEFAULT
        // signature, so the signed transaction is at most one vB smaller than what was paid for.
        for (rate, satPerVb) <- Seq(1000L -> 1L, 1001L -> 2L, 2000L -> 2L) do {
            val tx = built(Seq(coin(0x22, 1, 100000)), rate)
            val fee = 100000 - tx.outputs.map(_.value.satoshis.toLong).sum
            assert(fee >= tx.vsize * satPerVb && fee <= (tx.vsize + 1) * satPerVb, s"fee $fee")
        }
    }

    test("every input is signed under the funding output key by key path") {
        val coins = Seq(coin(0x22, 1, 60000), coin(0x33, 0, 50000))
        val tx = built(coins)
        assert(tx.inputs.map(_.previousOutput).toSet == coins.map(_.outpoint).toSet)
        val prevouts = PreviousOutputMap(coins.map { c =>
            c.outpoint -> TransactionOutput(Satoshis(c.amountSat), funding.taprootScriptPubKey)
        }.toMap)
        for i <- tx.inputs.indices do {
            val stack = tx.witness(i).stack
            assert(stack.map(_.size) == Vector(64), "one 64-byte SIGHASH_DEFAULT signature")
            val digest = TransactionSignatureSerializer.hashForSignature(
              TaprootTxSigComponent(tx, UInt32(i), prevouts, Policy.standardFlags),
              SIGHASH_DEFAULT,
              TaprootSerializationOptions(None, None, None)
            )
            assert(
              SchnorrPublicKey(funding.outputKey)
                  .verify(digest.bytes, SchnorrDigitalSignature(stack.head))
            )
        }
    }

    test("a foreign funding output, too little money or no coins is refused, not thrown") {
        val foreign = coin(0x22, 1, 100000).copy(scriptPubKey = wallet.identity.scriptPubKey)
        assert(build(Seq(foreign)).isLeft)
        assert(build(Seq(coin(0x22, 1, 30100))).isLeft)
        assert(build(Seq.empty).isLeft)
    }
}
