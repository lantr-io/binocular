package binocular

import binocular.bitcoin.{BitcoinHelpers, BlockInfo, RawTransactionInfo, TransactionInfo, VoutInfo}
import binocular.oracle.{reverse, BlockHeader, MerkleTree}
import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as MPF
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

/** Unit tests for the [OB-12] deposit-inclusion bundle.
  *
  * The bundle's four items are the `PegInRequest` mint redeemer, so each accepting test verifies
  * them exactly the way `peg_in.ak`'s mint handler does: the header hashes into the oracle's
  * `confirmed_blocks_root` MPF, the tx merkle proof reproduces the header's merkle root, and the
  * raw tx hashes to the proven txid.
  *
  * The deposit fixture is byte-identical to `bifrost/bitcoin.ak`'s `sample_deposit_tx`: vout 0 =
  * P2TR worth 100000 sat, vout 1 = the dual-key BFR beacon (`6a 43 "BFR" ‖ D(32) ‖ Q_auth(32)`).
  * The legacy single-key beacon (`6a 23 "BFR" ‖ key`) is REFUSED on-chain by `beacon_payload`, so
  * the bundle producer must refuse it too — a bundle for it would fail `deposit_binding_ok` at
  * submission.
  */
class PegInProofBundleTest extends AnyFunSuite {

    // bifrost/bitcoin.ak::sample_deposit_tx, verbatim. Non-witness, two outputs.
    private val depositTxHex =
        "020000000100000000000000000000000000000000000000000000000000000000000000000000000000ffffffff02a086010000000000225120bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb0000000000000000456a43424652ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00000000"

    // bifrost/bitcoin.ak::legacy_deposit_tx: the retired single-key beacon (6a 23 "BFR" ‖ Q_auth).
    private val legacyDepositTxHex =
        "020000000100000000000000000000000000000000000000000000000000000000000000000000000000ffffffff02a086010000000000225120bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb0000000000000000256a23424652cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00000000"

    private val refundKey = "dd" * 32
    private val authKey = "cc" * 32

    private def voutsOf(rawHex: String): Seq[VoutInfo] = {
        val raw = ByteString.fromHex(rawHex)
        binocular.watchtower.TreasuryMovementValidator
            .allOutputs(raw)
            .asScala
            .toSeq
            .zipWithIndex
            .map { case (out, i) => VoutInfo(i, out.scriptPubKey.toHex, 0.0) }
    }

    /** A single-tx block for `rawHex`: the merkle root IS the txid. */
    private case class Fixture(rawHex: String) {
        val raw: ByteString = ByteString.fromHex(rawHex)
        val txidLE: ByteString = BitcoinHelpers.getTxHash(raw)
        val txidDisplay: String = txidLE.reverse.toHex
        val headerHex: String =
            "00000000" + ("00" * 32) + txidLE.toHex + ("00" * 12)
        val blockHashLE: ByteString =
            BitcoinHelpers.blockHeaderHash(BlockHeader(ByteString.fromHex(headerHex)))
        val blockHashDisplay: String = blockHashLE.reverse.toHex
        val rawTxInfo: RawTransactionInfo =
            RawTransactionInfo(txidDisplay, txidDisplay, rawHex, Some(blockHashDisplay), 10)
        val block: BlockInfo = BlockInfo(
          hash = blockHashDisplay,
          height = 100,
          version = 0,
          merkleroot = txidDisplay,
          time = 0L,
          nonce = 0L,
          bits = "",
          difficulty = 0.0,
          previousblockhash = None,
          tx = Seq(TransactionInfo(txidDisplay, rawHex, voutsOf(rawHex)))
        )
        val mpf: OffChainMPF = OffChainMPF.empty.insert(blockHashLE, blockHashLE)

        def assemble(
            requestedVout: Option[Int],
            mpfOverride: OffChainMPF = mpf
        ): Either[PegInProofBundle.ProduceError, PegInProofBundle] =
            PegInProofBundle.assemble(rawTxInfo, block, headerHex, mpfOverride, requestedVout)
    }

    private val deposit = Fixture(depositTxHex)

    // --- the four [OB-12] items, verified the way peg_in.ak's mint handler does ----------------

    test("the bundle's four items satisfy the mint handler's checks") {
        val bundle = deposit.assemble(requestedVout = Some(0)).toOption.get

        // 1. The raw deposit tx: hashes to the txid the block merkle root commits to.
        assert(BitcoinHelpers.getTxHash(bundle.rawTxHex) == deposit.txidLE)

        // 2. The 80-byte block header.
        assert(bundle.blockHeader.size == 80)

        // 3. Tx merkle proof + index: reproduces the header's merkle root (bytes [36, 68)).
        val headerMerkleRoot = bundle.blockHeader.slice(36, 32)
        val calculated = MerkleTree.calculateMerkleRootFromProof(
          bundle.txIndex,
          deposit.txidLE,
          bundle.txInBlockMerklePath.toList
        )
        assert(calculated == headerMerkleRoot)

        // 4. MPF membership of the block hash against the oracle's confirmed_blocks_root, exactly
        //    as the mint handler verifies it: mpf.has(root, block_hash, block_hash, proof).
        val blockHash = BitcoinHelpers.blockHeaderHash(BlockHeader(bundle.blockHeader))
        assert(MPF(deposit.mpf.rootHash).has(blockHash, blockHash, bundle.mpfHeaderInclusionProof))
    }

    test("the convenience fields mirror deposit_binding_ok's reading of the deposit") {
        val bundle = deposit.assemble(requestedVout = Some(0)).toOption.get
        assert(bundle.pegInVout == 0)
        assert(bundle.pegInAmountSat == 100_000L)
        // Q_auth (the SECOND beacon key) is the user_source_chain_pub_key — not the refund key D.
        assert(bundle.userSourceChainPubKey == ByteString.fromHex(authKey))
        assert(bundle.userSourceChainPubKey != ByteString.fromHex(refundKey))
        assert(
          bundle.pegInUtxoId == deposit.txidLE ++ hex"00000000"
        )
    }

    test("auto-detection (txid-keyed path) finds the same deposit vout") {
        val bundle = deposit.assemble(requestedVout = None).toOption.get
        assert(bundle.pegInVout == 0)
        assert(bundle.pegInAmountSat == 100_000L)
    }

    // --- outpoint-keyed refusals ---------------------------------------------------------------

    test("an outpoint naming the OP_RETURN vout is refused — it is not the deposit output") {
        assert(
          deposit.assemble(requestedVout = Some(1)) ==
              Left(PegInProofBundle.VoutNotDeposit(deposit.txidDisplay, 1))
        )
    }

    test("an outpoint naming a vout the tx does not have is refused") {
        assert(
          deposit.assemble(requestedVout = Some(7)) ==
              Left(PegInProofBundle.VoutNotDeposit(deposit.txidDisplay, 7))
        )
    }

    test("the legacy single-key beacon is refused, exactly like on-chain beacon_payload") {
        val legacy = Fixture(legacyDepositTxHex)
        assert(
          legacy.assemble(requestedVout = Some(0)) ==
              Left(PegInProofBundle.NoBfrOpReturn(legacy.txidDisplay))
        )
    }

    test("a dual-key beacon anywhere but vout 1 is refused — on-chain reads vout 1 only") {
        // Swap the two outputs: P2TR at vout 1, beacon at vout 0.
        val fx = Fixture(depositTxHex)
        val swapped = fx.block.copy(tx =
            Seq(
              TransactionInfo(
                fx.txidDisplay,
                depositTxHex,
                voutsOf(depositTxHex).reverse.zipWithIndex.map { case (v, i) => v.copy(index = i) }
              )
            )
        )
        val res =
            PegInProofBundle.assemble(fx.rawTxInfo, swapped, fx.headerHex, fx.mpf, Some(1))
        assert(res == Left(PegInProofBundle.NoBfrOpReturn(fx.txidDisplay)))
    }

    test("a block the oracle has not confirmed yet is a structured error, not a crash") {
        assert(
          deposit.assemble(requestedVout = Some(0), mpfOverride = OffChainMPF.empty) ==
              Left(
                PegInProofBundle.BlockNotConfirmedByOracle(
                  deposit.txidDisplay,
                  deposit.blockHashDisplay
                )
              )
        )
    }
}
