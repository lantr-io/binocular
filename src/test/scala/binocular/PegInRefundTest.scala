package binocular

import binocular.bitcoin.{Bip86Wallet, BitcoinNetwork, PegInRefund, PeginTree, PeginTreeParams, Taproot}

import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script.{TaprootScriptPath, TapscriptControlBlock}
import org.bitcoins.core.protocol.transaction.{BaseTransaction, Transaction, TransactionOutPoint, WitnessTransaction}
import org.bitcoins.crypto.{DoubleSha256DigestBE, SchnorrDigitalSignature, SchnorrPublicKey}
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector

/** The depositor REFUND (spec TRF-75 to TRF-78, TRF-84, TRF-117, TRF-125).
  *
  * The refund is the only path that recovers a stranded deposit, it runs up to five days after the
  * deposit, and every mistake in it is silent until the coins are already unrecoverable. So the
  * digest is not merely round-tripped through sign-and-verify here: it is pinned against the
  * `depositor_refund` vector, computed by the pure-Python BIP-341 implementation that
  * `scripts/gen-taproot-vectors.py` carried at commit db4cb06.
  */
class PegInRefundTest extends AnyFunSuite {

    private def hex(s: String): ByteVector = ByteVector.fromValidHex(s)

    private val tupleA = TaprootVectors.tupleA
    private val Timeout = tupleA("pegin_refund_timeout_blocks").num.toInt // 720

    private val params = PeginTreeParams(
      y51 = TaprootVectors.hex(tupleA("y51")),
      yFederation = TaprootVectors.hex(tupleA("y_federation")),
      federationCsvBlocks = tupleA("federation_csv_blocks").num.toInt, // 144
      peginRefundTimeoutBlocks = Timeout
    )

    private val wallet = Bip86Wallet
        .fromMnemonic(("abandon " * 11) + "about", BitcoinNetwork.Testnet4)
        .fold(e => fail(e), w => w)

    private val depositor = wallet.identity

    /** TRF-76: the refund is paid to an address on the worker's own `m/86'/1'/0'` branch. */
    private val payout: ByteVector = wallet.funding.scriptPubKey

    private def treeWith(y51: ByteVector, qAuth: ByteVector): PeginTree =
        Taproot.peginTree(params.copy(y51 = y51), qAuth).fold(e => fail(e), t => t)

    private val tree = treeWith(params.y51, depositor.outputKey)

    /** Both the vector's deposit outpoint and this suite's. */
    private val deposit = TransactionOutPoint(DoubleSha256DigestBE.fromHex("11" * 32), UInt32.zero)
    private val DepositSat = 25000L

    private def refund(
        feeRateSatPerKvb: Long = 1000L,
        depositAmountSat: Long = DepositSat,
        payoutScriptPubKey: ByteVector = payout,
        timeoutBlocks: Int = Timeout,
        peginTree: PeginTree = tree
    ): Either[String, Transaction] = PegInRefund.build(
      peginTree,
      deposit,
      depositAmountSat,
      timeoutBlocks,
      payoutScriptPubKey,
      depositor,
      feeRateSatPerKvb
    )

    private def built(feeRateSatPerKvb: Long = 1000L): Transaction =
        refund(feeRateSatPerKvb).fold(e => fail(e), tx => tx)

    /** The witness stack in WIRE order: `<sig> <leaf> <control block>`. bitcoin-s stores it
      * top-first, so the wire order is its reverse.
      */
    private def wireWitness(tx: Transaction): Vector[ByteVector] =
        tx.asInstanceOf[WitnessTransaction].witness.witnesses.head.stack.toVector.reverse

    test("TRF-70, TRF-77, TRF-117: version 2, one input at nSequence = timeout, one payout") {
        val tx = built()
        assert(tx.version == Int32.two)
        assert(tx.lockTime == UInt32.zero)
        assert(tx.inputs.size == 1)
        assert(tx.inputs.head.previousOutput == deposit)
        // A BIP-68 block count carries no flag bits: the disable bit and the type bit are clear.
        assert(tx.inputs.head.sequence == UInt32(Timeout))
        assert(tx.outputs.size == 1)
        assert(tx.outputs.head.scriptPubKey.asmBytes == payout)
    }

    test("TRF-78: the witness is <sig> <refund leaf> <control block>, and survives the wire") {
        val tx = built()
        val stack = wireWitness(tx)
        assert(stack.size == 3)
        // A SIGHASH_DEFAULT BIP-340 signature is exactly 64 bytes; a sighash byte would make 65.
        assert(stack(0).length == 64)
        assert(stack(1) == tree.refundLeaf)
        assert(stack(2) == tree.refundControlBlock)
        assert(wireWitness(Transaction.fromBytes(tx.bytes)) == stack)
    }

    test("TRF-78: the control block commits the refund leaf to the deposit's output key") {
        assert(
          TaprootScriptPath.verifyTaprootCommitment(
            TapscriptControlBlock.fromBytes(tree.refundControlBlock),
            tree.scriptPubKey,
            Taproot.leafHash(tree.refundLeaf)
          )
        )
    }

    test("the signature verifies under Q_auth, and not under the untweaked internal key") {
        val tx = built()
        val sig = SchnorrDigitalSignature.fromBytes(wireWitness(tx).head)
        val digest = PegInRefund.sighash(tree, DepositSat, tx)
        assert(SchnorrPublicKey(depositor.outputKey).verify(digest, sig))
        // The refund leaf names Q_auth, the TWEAKED key. Signing with the raw HD child would
        // produce a well-formed transaction that no node accepts.
        assert(!SchnorrPublicKey(depositor.internalKey).verify(digest, sig))
    }

    test("the digest commits to the payout script") {
        val mine = built()
        val elsewhere =
            refund(payoutScriptPubKey = depositor.scriptPubKey).fold(e => fail(e), t => t)
        assert(
          PegInRefund.sighash(tree, DepositSat, mine) !=
              PegInRefund.sighash(tree, DepositSat, elsewhere)
        )
    }

    test("TRF-117: the fee is the MEASURED vsize at the quoted rate, rounded up") {
        for rate <- Seq(1000L, 1501L) do
            val tx = built(rate)
            val feeSat = DepositSat - tx.outputs.head.value.satoshis.toLong
            // The dummy signature the fee was measured with is the size of the real one, so the
            // vsize of the signed transaction is the vsize the fee was taken over.
            withClue(s"$rate sat/kvB over ${tx.vsize} vB: ") {
                assert(feeSat == (tx.vsize * rate + 999) / 1000)
            }
    }

    test("TRF-117: a payout below 330 sat is refused rather than broadcast") {
        val feeSat = DepositSat - built().outputs.head.value.satoshis.toLong
        // The output value is 8 bytes whatever it holds, so moving the deposit amount cannot move
        // the vsize, and the fee above is the fee of both of these.
        assert(refund(depositAmountSat = feeSat + 330).isRight)
        assert(refund(depositAmountSat = feeSat + 329).isLeft)
        assert(refund(depositAmountSat = feeSat - 1).isLeft, "a fee above the deposit is a Left")
    }

    test("a negative fee rate and a timeout outside 1..0xffff are refused, never thrown") {
        assert(refund(feeRateSatPerKvb = -1).isLeft)
        for bad <- Seq(0, -1, 0x10000) do
            withClue(s"timeout $bad: ")(assert(refund(timeoutBlocks = bad).isLeft))
        // A timeout that disagrees with the leaf's OP_CSV delay would fail at broadcast, days on.
        assert(refund(timeoutBlocks = 719).isLeft)
    }

    test("TRF-116, TRF-125: only the deposit's own Y_51 rebuilds a tree that opens the deposit") {
        // Y_51 rotates daily; the deposit's script is the only record of which value it was made
        // under. `y_federation` stands in for a rotated key: a valid x-only key that is not Y_51.
        val rotated = TaprootVectors.hex(tupleA("y_federation"))
        val wrong = treeWith(rotated, depositor.outputKey)
        assert(wrong.scriptPubKey != tree.scriptPubKey, "a rotated Y_51 is a different address")
        assert(wrong.refundLeaf == tree.refundLeaf, "only the internal key moved")

        def opens(candidate: PeginTree): Boolean = TaprootScriptPath.verifyTaprootCommitment(
          TapscriptControlBlock.fromBytes(candidate.refundControlBlock),
          tree.scriptPubKey,
          Taproot.leafHash(tree.refundLeaf)
        )
        assert(opens(tree))
        assert(!opens(wrong))
    }

    test("TRF-84: the refund of tuple A reproduces the independently computed vector") {
        // `depositor_refund` from the `transactions` section of taproot-pegin-vectors.json at
        // commit db4cb06, produced by the pure-Python BIP-341 implementation in
        // scripts/gen-taproot-vectors.py, which shares no code with bitcoin-s. Recover it with
        //   git show db4cb06:src/test/resources/fixtures/taproot-pegin-vectors.json
        // The refund spends 25000 sat of tuple A's peg-in output at 1 sat/vB, so its 138 sat fee
        // is its 138 vB size, and it pays the remainder to the depositor's own P2TR.
        // Split at field boundaries, so a miscounted line cannot hide inside the string.
        val UnsignedHex =
            "02000000" + "01" + "11" * 32 + "00000000" + "00" + "d0020000" + // version, the input
                "01" + "1e61000000000000" + "22" + // one output, 24862 sat, a 34-byte script
                "512017e60637618d309c2f639616ee95da701d40b8ab5315543c6da6e82bb03ad8c2" +
                "00000000" // locktime
        val Sighash = "8a1a8e8ba6c521748dc4ad7038de5d091faf5a9ecf355cf8a66719f2cf36912a"
        val VectorVsize = 138L
        val VectorPayoutSat = 24862L

        val qAuth = TaprootVectors.hex(tupleA("q_auth"))
        val vectorTree = Taproot.peginTree(params, qAuth).fold(e => fail(e), t => t)
        assert(vectorTree.scriptPubKey.asmBytes.toHex == tupleA("script_pub_key").str)

        // The signer is this suite's wallet, not the vector's key: the BIP-341 digest commits to
        // the leaf, never to who signs it, so the body and the digest below are the vector's.
        val tx = refund(peginTree = vectorTree, payoutScriptPubKey = hex("5120" + qAuth.toHex))
            .fold(e => fail(e), t => t)

        assert(tx.vsize == VectorVsize)
        assert(tx.outputs.head.value.satoshis.toLong == VectorPayoutSat)
        assert(BaseTransaction(tx.version, tx.inputs, tx.outputs, tx.lockTime).hex == UnsignedHex)
        assert(PegInRefund.sighash(vectorTree, DepositSat, tx).toHex == Sighash)
    }
}
