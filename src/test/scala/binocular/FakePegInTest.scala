package binocular

import binocular.attack.FakePegIn
import binocular.bitcoin.BitcoinHelpers
import binocular.oracle.MerkleTree
import org.scalatest.funsuite.AnyFunSuite

class FakePegInTest extends AnyFunSuite {

    test("fabricated deposit tx is legacy, encodes 100 BTC, and carries BFR + P2TR scripts") {
        val d = FakePegIn.buildDeposit(140000)
        assert(!BitcoinHelpers.isWitnessTransaction(d.rawTx), "must be a legacy (non-witness) tx")
        assert(d.amountSat == BigInt(10_000_000_000L))
        // output 0 is the 100 BTC P2TR
        assert(BitcoinHelpers.outputValueSat(d.rawTx, 0) == BigInt(10_000_000_000L))
        assert(d.p2trScript.toHex.startsWith("5120"))
        // The one-key beacon: OP_RETURN PUSH35 "BFR" ‖ Q_auth(32) = 37 bytes. The width is the
        // thing PegInProofBundle and beacon_payload both key on, so pin it, not just the prefix.
        assert(d.opReturnScript.toHex.startsWith("6a23424652"))
        assert(d.opReturnScript.size == 37)
        // txid is the double-sha of the (legacy) raw tx, deterministic
        assert(d.txid == BitcoinHelpers.getTxHash(d.rawTx))
        assert(FakePegIn.buildDeposit(140000).txid == d.txid) // deterministic
        assert(FakePegIn.buildDeposit(140001).txid != d.txid) // distinct per height
    }

    test("commitment Merkle proof for the deposit verifies back to the root") {
        val c = FakePegIn.commitment(140000)
        assert(c.leaves.size == 4)
        assert(c.leaves(c.depositIndex) == c.pegIn.txid)
        val recomputed =
            MerkleTree.calculateMerkleRootFromProof(c.depositIndex, c.pegIn.txid, c.proof)
        assert(recomputed == c.merkleRoot)
    }
}
