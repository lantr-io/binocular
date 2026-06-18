package binocular.attack

import binocular.oracle.MerkleTree
import scalus.uplc.builtin.ByteString
import java.time.Instant

/** Pure renderer that turns a freshly mined rogue block into a detailed, human-readable report
  * exposing the valid proof-of-work, the mining cost, the Merkle commitment, and the fabricated
  * 100-BTC peg-in payload. No I/O — returns a multi-line String.
  */
object RogueBlockReport {

    /** Render a 32-byte hash in Bitcoin display order (reverse of internal sha256d order). */
    private def display(h: ByteString): String =
        ByteString.fromArray(h.bytes.reverse).toHex

    /** Format a satoshi amount as BTC with 8 decimal places (string math, no Double). */
    private def btc(sat: BigInt): String = {
        val s = sat.toString.reverse.padTo(9, '0').reverse
        s.dropRight(8) + "." + s.takeRight(8)
    }

    /** Count leading zero bytes of a 32-byte display-order hash. */
    private def leadingZeroBytes(displayHash: ByteString): Int = {
        val b = displayHash.bytes
        var i = 0
        while i < b.length && b(i) == 0.toByte do i += 1
        i
    }

    def render(
        b: RogueMiner.MinedBlock,
        nowSeconds: BigInt,
        powLimit: BigInt,
        oracleBestChainwork: BigInt,
        oracleBestTip: String
    ): String = {
        val sb = new StringBuilder

        val blockDisplayHash = ByteString.fromArray(b.summary.hash.bytes.reverse)
        val displayHashHex = display(b.summary.hash)

        // Header line
        sb.append(s"🩸 ROGUE BLOCK MINED  height=${b.height}  hash=$displayHashHex\n")

        // Parent / fork
        sb.append(s"  Parent / fork:\n")
        sb.append(s"    parent hash : ${display(b.parentHash)}\n")
        sb.append(s"    note        : extends a competing branch off the honest chain\n")

        // Proof-of-work
        // Min-difficulty blocks carry exactly the compact encoding of powLimit (what
        // getNextWorkRequired emits for the testnet 20-min rule). Comparing decoded
        // targets wouldn't work: compact bits keep only 3 mantissa bytes, so the
        // decoded target is always slightly below the raw powLimit.
        val diffFlag =
            if b.bits == binocular.bitcoin.BitcoinHelpers.targetToCompactByteString(powLimit) then
                "min-difficulty (powLimit)"
            else "difficulty>1"
        val zeroBytes = leadingZeroBytes(blockDisplayHash)
        sb.append(s"  Proof-of-work:\n")
        sb.append(s"    bits=${b.bits.toHex}  target=0x${b.target.toString(16)}  $diffFlag\n")
        sb.append(s"    PoW: hash <= target ✓  (leading zero bytes: $zeroBytes)\n")
        sb.append(s"    nonce=${b.nonce}\n")

        // Mining cost
        val rate = b.hashesTried.toDouble / math.max(1L, b.miningMillis).toDouble / 1000.0
        sb.append(s"  Mining cost:\n")
        sb.append(
          f"    mined in ${b.miningMillis} ms  (~${b.hashesTried} hashes, ~$rate%.2f MH/s)\n"
        )

        // Timestamp
        val deltaMin = (b.summary.timestamp - nowSeconds) / 60
        val signedMin = if deltaMin >= 0 then s"+$deltaMin min" else s"$deltaMin min"
        sb.append(s"  Timestamp:\n")
        sb.append(
          s"    ${Instant.ofEpochSecond(b.summary.timestamp.toLong)} UTC  ($signedMin vs now)\n"
        )

        // Merkle commitment
        val c = b.commitment
        sb.append(s"  Merkle commitment:\n")
        sb.append(s"    root: ${display(c.merkleRoot)}\n")
        c.leaves.zipWithIndex.foreach { case (leaf, idx) =>
            val label =
                if idx == 0 then "coinbase"
                else if idx == c.depositIndex then "★ fake peg-in deposit"
                else "filler"
            sb.append(s"    leaf[$idx] ($label): ${display(leaf)}\n")
        }
        sb.append(s"    inclusion proof for deposit (index=${c.depositIndex}):\n")
        c.proof.zipWithIndex.foreach { case (sibling, idx) =>
            sb.append(s"      sibling[$idx]: ${display(sibling)}\n")
        }
        val reDerived =
            MerkleTree.calculateMerkleRootFromProof(
              c.depositIndex,
              c.pegIn.txid,
              c.proof
            ) == c.merkleRoot
        sb.append(
          s"    proof re-derives root via MerkleTree.calculateMerkleRootFromProof: $reDerived\n"
        )

        // Fabricated peg-in (the payload)
        val p = c.pegIn
        sb.append(s"  Fabricated peg-in (the payload):\n")
        sb.append(s"    ${btc(p.amountSat)} BTC → P2TR treasury ${p.treasuryQ.toHex}\n")
        sb.append(s"    depositor (x-only): ${p.depositorXOnly.toHex}\n")
        sb.append(s"    OP_RETURN: ${p.opReturnScript.toHex}\n")
        sb.append(s"    deposit txid: ${display(p.txid)}\n")
        sb.append(s"    raw deposit tx: ${p.rawTx.toHex}\n")

        // Raw header
        sb.append(s"  Raw header:\n")
        sb.append(s"    ${b.header.bytes.toHex}\n")

        // Chainwork / verdict
        sb.append(s"  Chainwork / verdict:\n")
        sb.append(
          s"    this block adds chainwork ${b.blockProof}; Eve branch is lighter than the honest " +
              s"best chain ($oracleBestChainwork, tip $oracleBestTip)\n"
        )
        sb.append(
          s"    → valid PoW, FAKE content: the oracle ACCEPTS this into the fork tree but never " +
              s"promotes it (loses the chainwork race), so the 100 BTC peg-in can never be used " +
              s"to mint fBTC.\n"
        )

        sb.toString
    }
}
