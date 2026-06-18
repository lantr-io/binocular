package binocular.attack

import binocular.bitcoin.BitcoinHelpers
import binocular.oracle.MerkleTree
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Builtins.{integerToByteString, sha2_256}
import scala.collection.Seq

/** A fabricated Bitcoin peg-in DEPOSIT transaction (100 BTC) — the artifact the bridge's
  * PegInProofBundle would recognize: a P2TR output to the treasury plus a "BFR" OP_RETURN naming
  * the depositor. Placeholder (fabricated) treasury and depositor keys — this is for the
  * adversarial demo, not a real deposit.
  */
case class FakePegIn(
    amountSat: BigInt, // 10_000_000_000 = 100 BTC
    treasuryQ: ByteString, // 32-byte x-only (fabricated placeholder)
    depositorXOnly: ByteString, // 32-byte x-only (fabricated "Eve")
    p2trScript: ByteString, // 5120 ++ treasuryQ
    opReturnScript: ByteString, // 6a23 ++ 424652 ++ depositorXOnly
    rawTx: ByteString, // full legacy tx bytes
    txid: ByteString // getTxHash(rawTx), internal (LE) order
)

/** What a rogue block commits to: the fabricated tx set, the Merkle root that goes into the block
  * header, and a real inclusion proof for the deposit tx.
  */
case class RogueCommitment(
    merkleRoot: ByteString,
    leaves: Seq[ByteString], // txids in tree order: [coinbase, deposit, filler1, filler2]
    depositIndex: Int, // index of the deposit leaf (1)
    proof: Seq[ByteString], // makeMerkleProof(depositIndex)
    pegIn: FakePegIn
)

object FakePegIn {
    val AmountSat: BigInt = BigInt(10_000_000_000L) // 100 BTC

    // Fabricated treasury x-only key (placeholder)
    private val TreasuryQ: ByteString =
        sha2_256(ByteString.fromString("bifrost-treasury-Q-PLACEHOLDER"))

    // Fabricated depositor x-only key ("Eve")
    private val DepositorXOnly: ByteString =
        sha2_256(ByteString.fromString("eve-depositor-xonly"))

    /** Build the fabricated 100-BTC peg-in deposit tx for a given rogue height. The funding
      * outpoint is keyed by height so each block's deposit txid (and thus the block's Merkle root)
      * is distinct.
      */
    def buildDeposit(height: BigInt): FakePegIn = {
        // version: 02000000 (LE uint32 = 2)
        val version = integerToByteString(false, 4, 2)

        // vin count: 1
        val vinCount = ByteString.fromHex("01")

        // vin[0]: prevTxid (32 bytes, height-keyed) + prevVout(0) + empty scriptSig + sequence
        val prevTxid = sha2_256(ByteString.fromString(s"eve-funding-$height"))
        val prevVout = integerToByteString(false, 4, 0)
        val scriptSigLen = ByteString.fromHex("00")
        val sequence = ByteString.fromHex("ffffffff")

        // vout count: 2
        val voutCount = ByteString.fromHex("02")

        // vout[0] — P2TR 100 BTC
        val p2trScript = ByteString.fromHex("5120") ++ TreasuryQ
        val amount0 = integerToByteString(false, 8, AmountSat)
        val scriptLen0 = ByteString.fromHex("22") // 34 bytes: 2 (5120) + 32 (key)
        val vout0 = amount0 ++ scriptLen0 ++ p2trScript

        // vout[1] — OP_RETURN BFR
        val opReturnScript = ByteString.fromHex("6a23424652") ++ DepositorXOnly
        val amount1 = integerToByteString(false, 8, 0)
        val scriptLen1 = ByteString.fromHex("25") // 37 bytes: 5 (6a23424652) + 32 (key)
        val vout1 = amount1 ++ scriptLen1 ++ opReturnScript

        // locktime: 00000000
        val locktime = integerToByteString(false, 4, 0)

        val rawTx =
            version ++
                vinCount ++
                prevTxid ++
                prevVout ++
                scriptSigLen ++
                sequence ++
                voutCount ++
                vout0 ++
                vout1 ++
                locktime

        val txid = BitcoinHelpers.getTxHash(rawTx)

        FakePegIn(
          amountSat = AmountSat,
          treasuryQ = TreasuryQ,
          depositorXOnly = DepositorXOnly,
          p2trScript = p2trScript,
          opReturnScript = opReturnScript,
          rawTx = rawTx,
          txid = txid
        )
    }

    /** Build the full rogue commitment (tx set + Merkle root + inclusion proof). */
    def commitment(height: BigInt): RogueCommitment = {
        val pegIn = buildDeposit(height)

        val coinbase = sha2_256(sha2_256(ByteString.fromString(s"rogue-coinbase-$height")))
        val deposit = pegIn.txid
        val filler1 = sha2_256(sha2_256(ByteString.fromString(s"rogue-filler-1-$height")))
        val filler2 = sha2_256(sha2_256(ByteString.fromString(s"rogue-filler-2-$height")))

        val leaves = Seq(coinbase, deposit, filler1, filler2)
        val tree = MerkleTree.fromHashes(leaves)
        val merkleRoot = tree.getMerkleRoot
        val depositIndex = 1
        val proof = tree.makeMerkleProof(depositIndex)

        RogueCommitment(
          merkleRoot = merkleRoot,
          leaves = leaves,
          depositIndex = depositIndex,
          proof = proof,
          pegIn = pegIn
        )
    }
}
