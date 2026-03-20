package binocular

import binocular.BitcoinHelpers.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.compiler.Options
import scalus.testing.kit.ScalusTest
import scalus.uplc.Term.asTerm
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.{Builtins, ByteString}
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.{Constant, PlutusV3, Term}
import upickle.default.*

import java.nio.file.{Files, Path}

class BitcoinHelpersTest extends AnyFunSuite with ScalusTest with ScalaCheckPropertyChecks {

    test("parseCoinbaseTx") {
        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2503233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100ffffffff02f595814a000000001976a914edf10a7fac6b32e24daa5305c723f3de58db1bc888ac0000000000000000266a24aa21a9edfaa194df59043645ba0f58aad74bfd5693fa497093174d12a4bb3b0574a878db0120000000000000000000000000000000000000000000000000000000000000000000000000"
        val scriptSig = parseCoinbaseTxScriptSig(coinbaseTx)
        assert(
          scriptSig ==
              hex"03233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100"
        )
    }

    test("construct CoinbaseTx") {
        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2503233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100ffffffff02f595814a000000001976a914edf10a7fac6b32e24daa5305c723f3de58db1bc888ac0000000000000000266a24aa21a9edfaa194df59043645ba0f58aad74bfd5693fa497093174d12a4bb3b0574a878db0120000000000000000000000000000000000000000000000000000000000000000000000000"
        val coinbase = Bitcoin.makeCoinbaseTxFromByteString(coinbaseTx)
        val txHash = getCoinbaseTxHash(coinbase)
        assert(
          txHash ==
              hex"31e9370f45eb48f6f52ef683b0737332f09f1cead75608021185450422ec1a71".reverse
        )
    }

    test("parseBlockHeightFromScriptSig") {
        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2503233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100ffffffff02f595814a000000001976a914edf10a7fac6b32e24daa5305c723f3de58db1bc888ac0000000000000000266a24aa21a9edfaa194df59043645ba0f58aad74bfd5693fa497093174d12a4bb3b0574a878db0120000000000000000000000000000000000000000000000000000000000000000000000000"
        val scriptSig = parseCoinbaseTxScriptSig(coinbaseTx)
        val blockHeight = parseBlockHeightFromScriptSig(scriptSig)
        assert(blockHeight == BigInt(538403))
    }

    test("getTxHash") {
        val coinbaseTx =
            hex"010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2503233708184d696e656420627920416e74506f6f6c373946205b8160a4256c0000946e0100ffffffff02f595814a000000001976a914edf10a7fac6b32e24daa5305c723f3de58db1bc888ac0000000000000000266a24aa21a9edfaa194df59043645ba0f58aad74bfd5693fa497093174d12a4bb3b0574a878db0120000000000000000000000000000000000000000000000000000000000000000000000000"
        val txHash = getTxHash(coinbaseTx)
        assert(
          txHash ==
              hex"31e9370f45eb48f6f52ef683b0737332f09f1cead75608021185450422ec1a71".reverse
        )
    }

    test("compactBitsToTarget") {
        // 0 exponent
        assert(
          compactBitsToTarget(hex"0002f128".reverse) ==
              BigInt("0", 16)
        )
        // real bits from block 867936
        assert(
          compactBitsToTarget(hex"0202f128".reverse) ==
              BigInt("00000000000000000000000000000000000000000000000000000000000002f1", 16)
        )
        assert(
          compactBitsToTarget(hex"1a030ecd".reverse) ==
              BigInt("000000000000030ecd0000000000000000000000000000000000000000000000", 16)
        )
        assert(
          compactBitsToTarget(hex"1d00ffff".reverse) ==
              BigInt("00000000ffff0000000000000000000000000000000000000000000000000000", 16)
        )
        // large exponent (exceeds mainnet PowLimit but is valid math - PowLimit check is in validateBlock)
        assert(
          compactBitsToTarget(hex"1e00ffff".reverse) ==
              BigInt("ffff000000000000000000000000000000000000000000000000000000", 16)
        )
    }

    test("targetToCompactBits") {
        // Test cases from Bitcoin Core's BOOST_AUTO_TEST_CASE(bignum_SetCompact)
        // These verify that targetToCompactBits (GetCompact) produces the expected 4-byte compact representation

        // Zero target
        assert(targetToCompactBits(BigInt(0)) == BigInt(0))

        // Case: target = 0x12 -> GetCompact = 0x01120000
        assert(targetToCompactBits(BigInt(0x12)) == BigInt(0x01120000L))

        // Case: target = 0x80 -> GetCompact = 0x02008000 (sign bit handling)
//        assertEquals(targetToCompactBits(BigInt(0x80)), BigInt(0x02008000L))

        // Case: target = 0x1234 -> GetCompact = 0x02123400
        assert(targetToCompactBits(BigInt(0x1234)) == BigInt(0x02123400L))

        // Case: target = 0x123456 -> GetCompact = 0x03123456
        assert(targetToCompactBits(BigInt(0x123456)) == BigInt(0x03123456L))

        // Case: target = 0x12345600 -> GetCompact = 0x04123456
        assert(targetToCompactBits(BigInt(0x12345600L)) == BigInt(0x04123456L))

//         Case: target = 0x92340000 -> GetCompact = 0x05009234
//        assertEquals(targetToCompactBits(BigInt(0x92340000L)), BigInt(0x05009234L))

        // Large number case from the test
        // target with leading 0x1234560000... -> GetCompact = 0x20123456
        val largeTarget =
            BigInt("1234560000000000000000000000000000000000000000000000000000000000", 16)
        assert(targetToCompactBits(largeTarget) == BigInt(0x20123456L))
    }

    test("TwoTo256 constant is correct") {
        // Verify that the precomputed TwoTo256 constant equals 2^256
        val computed = BigInt(2).pow(256)
        val constant = TwoTo256
        assert(constant == computed, "TwoTo256 constant must equal 2^256")
    }

    test("chainwork calculation matches Bitcoin Core GetBlockProof") {
        // This test verifies that chainwork (block proof) calculation matches Bitcoin Core's GetBlockProof()
        // Reference: Bitcoin Core src/chain.cpp::GetBlockProof()
        // Correct formula: work = 2^256 / (target + 1)

        // Test case 1: Real Bitcoin mainnet block 865494
        // bits: 0x17030ecd from block header
        val bits1 = hex"17030ecd".reverse
        val target1 = compactBitsToTarget(bits1)

        // Calculate using CORRECT Bitcoin Core formula: 2^256 / (target + 1)
        val twoTo256 = BigInt(2).pow(256)
        val correctWork1 = twoTo256 / (target1 + BigInt(1))

        // Calculate using calculateBlockProof (should match correct formula)
        val actualWork1 = calculateBlockProof(target1)

        // Verify that calculateBlockProof matches the correct formula
        assert(
          actualWork1 ==
              correctWork1,
          "calculateBlockProof should match Bitcoin Core formula: 2^256 / (target + 1)"
        )

        // Test case 2: Bitcoin genesis block difficulty (easiest)
        // bits: 0x1d00ffff (486604799 in decimal)
        val bits2 = hex"1d00ffff".reverse
        val target2 = compactBitsToTarget(bits2)

        val correctWork2 = twoTo256 / (target2 + BigInt(1))
        val actualWork2 = calculateBlockProof(target2)

        assert(
          actualWork2 ==
              correctWork2,
          "Genesis block work should match Bitcoin Core formula"
        )

        // Test case 3: Very high difficulty (recent blocks)
        val bits3 = hex"0202f128".reverse // From test block 867936
        val target3 = compactBitsToTarget(bits3)

        val correctWork3 = twoTo256 / (target3 + BigInt(1))
        val actualWork3 = calculateBlockProof(target3)

        assert(
          actualWork3 ==
              correctWork3,
          "High difficulty work should match Bitcoin Core formula"
        )
    }

    test("Block Header hash") {
        val blockHeader = BlockHeader(
          hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )
        val hash = blockHeaderHash(blockHeader)
        assert(
          hash.reverse ==
              hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c"
        )
    }

    test("Block Header hash satisfies proof of work") {
        val blockHeader = BlockHeader(
          hex"000000302b974c15e2ef994183f9806c5be9c61e74abc512a14301000000000000000000aff4af5b1dcc2b8754db824b9911818b65913dc262c295f060abb45c6c1d7ee749f90b67cd0e0317f9cc7dac"
        )
        val hash = blockHeaderHash(blockHeader)
        val target = compactBitsToTarget(hex"17030ecd".reverse)
        val proofOfWork = Builtins.byteStringToInteger(false, hash)
        assert(
          hash ==
              hex"00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c".reverse
        )
        assert(
          target ==
              BigInt("000000000000000000030ecd0000000000000000000000000000000000000000", 16)
        )
        assert(proofOfWork <= target, s"proofOfWork: $proofOfWork, target: $target")
    }

    test("insertReverseSorted") {
        forAll { (xs: Seq[BigInt], x: BigInt) =>
            val sorted = xs.sorted(using Ordering[BigInt].reverse)
            val sortedList = prelude.List.from(sorted)
            val inserted = insertReverseSorted(x, sortedList)
            val expected = (sorted :+ x).sorted(using Ordering[BigInt].reverse)
            assert(inserted == prelude.List.from(expected))
        }
    }

    test("merkleRootFromInclusionProof - single transaction") {
        val txHash = hex"abcd1234567890abcdef1234567890abcdef1234567890abcdef1234567890ab"
        val emptyProof = prelude.List.empty[ByteString]
        val result = merkleRootFromInclusionProof(emptyProof, txHash, BigInt(0))
        assert(result == txHash)
    }

    test("merkleRootFromInclusionProof - two transactions, left") {
        val leftTxHash = hex"abcd1234567890abcdef1234567890abcdef1234567890abcdef1234567890ab"
        val rightTxHash = hex"1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcd"
        val proof = prelude.List(rightTxHash)
        val expectedRoot = Builtins.sha2_256(Builtins.sha2_256(leftTxHash ++ rightTxHash))
        val result = merkleRootFromInclusionProof(proof, leftTxHash, BigInt(0))
        assert(result == expectedRoot)
    }

    test("merkleRootFromInclusionProof - two transactions, right") {
        val leftTxHash = hex"abcd1234567890abcdef1234567890abcdef1234567890abcdef1234567890ab"
        val rightTxHash = hex"1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcd"
        val proof = prelude.List(leftTxHash)
        val expectedRoot = Builtins.sha2_256(Builtins.sha2_256(leftTxHash ++ rightTxHash))
        val result = merkleRootFromInclusionProof(proof, rightTxHash, BigInt(1))
        assert(result == expectedRoot)
    }

    test("merkleRootFromInclusionProof - four transactions, various positions") {
        val tx0 = hex"0000000000000000000000000000000000000000000000000000000000000000"
        val tx1 = hex"1111111111111111111111111111111111111111111111111111111111111111"
        val tx2 = hex"2222222222222222222222222222222222222222222222222222222222222222"
        val tx3 = hex"3333333333333333333333333333333333333333333333333333333333333333"

        // Build tree manually
        val hash01 = Builtins.sha2_256(Builtins.sha2_256(tx0 ++ tx1))
        val hash23 = Builtins.sha2_256(Builtins.sha2_256(tx2 ++ tx3))
        val root = Builtins.sha2_256(Builtins.sha2_256(hash01 ++ hash23))

        // Test tx0 (index 0): proof should be [tx1, hash23]
        val proof0 = prelude.List.from(Seq(tx1, hash23))
        val result0 = merkleRootFromInclusionProof(proof0, tx0, BigInt(0))
        assert(result0 == root)

        // Test tx1 (index 1): proof should be [tx0, hash23]
        val proof1 = prelude.List.from(Seq(tx0, hash23))
        val result1 = merkleRootFromInclusionProof(proof1, tx1, BigInt(1))
        assert(result1 == root)

        // Test tx2 (index 2): proof should be [tx3, hash01]
        val proof2 = prelude.List.from(Seq(tx3, hash01))
        val result2 = merkleRootFromInclusionProof(proof2, tx2, BigInt(2))
        assert(result2 == root)

        // Test tx3 (index 3): proof should be [tx2, hash01]
        val proof3 = prelude.List.from(Seq(tx2, hash01))
        val result3 = merkleRootFromInclusionProof(proof3, tx3, BigInt(3))
        assert(result3 == root)
    }

    test("merkleRootFromInclusionProof - real Bitcoin data") {
        // Use the existing test data from the "Evaluate" test
        val block = read[BitcoinBlock](
          Files.readString(
            Path.of("00000000000000000002cfdedd8358532b2284bc157e1352dbc8682b2067fb0c.json")
          )
        )

        val txHashes = block.tx.map(h => ByteString.fromHex(h).reverse)
        val merkleTree = MerkleTree.fromHashes(txHashes)
        val expectedMerkleRoot = merkleTree.getMerkleRoot

        // Test coinbase transaction (index 0)
        val coinbaseHash = txHashes.head
        val merkleProof = merkleTree.makeMerkleProof(0)
        val proofData = prelude.List.from(merkleProof)

        val computedRoot = merkleRootFromInclusionProof(
          proofData,
          coinbaseHash,
          BigInt(0)
        )

        assert(computedRoot == expectedMerkleRoot)

        // Test a few other transactions if there are more than one
        if txHashes.length > 1 then {
            val lastTxHash = txHashes.last
            val lastIndex = txHashes.length - 1
            val lastMerkleProof = merkleTree.makeMerkleProof(lastIndex)
            val lastProofData = prelude.List.from(lastMerkleProof)

            val lastComputedRoot = merkleRootFromInclusionProof(
              lastProofData,
              lastTxHash,
              BigInt(lastIndex)
            )

            assert(lastComputedRoot == expectedMerkleRoot)
        }
    }

    test("merkleRootFromInclusionProof - edge cases") {
        val txHash = hex"abcd1234567890abcdef1234567890abcdef1234567890abcdef1234567890ab"

        // Empty proof with index 0 should return the hash itself
        val emptyProof = prelude.List.empty[ByteString]
        val result = merkleRootFromInclusionProof(emptyProof, txHash, BigInt(0))
        assert(result == txHash)
    }

    test("getNextWorkRequired - difficulty adjustment timing") {
        val currentTarget = hex"1d00ffff".reverse
        val blockTime = BigInt(1234567890)
        val firstBlockTime =
            BigInt(
              1234567890 - 2016 * 600 / 2
            ) // 2016 blocks * 10 blocks in half the time (should double the difficulty)

        // Test block heights that should NOT trigger difficulty adjustment
        val noAdjustmentHeights = Seq(0, 1, 100, 1000, 2014) // Not at 2016 interval
        for height <- noAdjustmentHeights do
            val result = getNextWorkRequired(
              height,
              currentTarget,
              blockTime,
              firstBlockTime,
              PowLimit
            )
            assert(
              result ==
                  currentTarget,
              s"Height $height should not trigger difficulty adjustment"
            )

        // Test block heights that SHOULD trigger difficulty adjustment
        val adjustmentHeights = Seq(2015, 4031, 6047) // Heights where (height + 1) % 2016 == 0
        for height <- adjustmentHeights do
            val result = getNextWorkRequired(
              height,
              currentTarget,
              blockTime,
              firstBlockTime,
              PowLimit
            )
            // Result should be different from currentTarget (actually calculated)
            assert(result != currentTarget, s"Height $height should trigger difficulty adjustment")
    }

    test("getNextWorkRequired - operator precedence regression test") {
        // This test specifically checks for the operator precedence bug
        // If the bug existed: nHeight + 1 % 2016 == 0 would be nHeight + (1 % 2016) == 0
        // which means nHeight + 1 == 0, only true for height -1 (impossible)

        val currentTarget = hex"1d00ffff".reverse // smallest difficulty
        val blockTime = BigInt(1234567890)
        val firstBlockTime =
            BigInt(
              1234567890 - 2016 * 600 / 2
            ) // Double block production, should make difficulty harder

        // Height 2015: (2015 + 1) % 2016 = 0 (should adjust)
        // With bug: 2015 + (1 % 2016) = 2015 + 1 = 2016 ≠ 0 (would not adjust)
        val result2015 =
            getNextWorkRequired(2015, currentTarget, blockTime, firstBlockTime, PowLimit)
        assert(result2015 != currentTarget, "Height 2015 should trigger difficulty adjustment")

        // Height 4031: (4031 + 1) % 2016 = 0 (should adjust)
        val result4031 =
            getNextWorkRequired(4031, currentTarget, blockTime, firstBlockTime, PowLimit)
        assert(result4031 != currentTarget, "Height 4031 should trigger difficulty adjustment")

        // Height 1000: (1000 + 1) % 2016 = 1001 ≠ 0 (should not adjust)
        val result1000 =
            getNextWorkRequired(1000, currentTarget, blockTime, firstBlockTime, PowLimit)
        assert(
          result1000 ==
              currentTarget,
          "Height 1000 should not trigger difficulty adjustment"
        )
    }

    test("targetToCompactBits V1 vs V2 (findFirstSetBit) - CEK ExUnits comparison") {
        // TODO: better test coverage
        given Options = Options.release
        given PlutusVM = PlutusVM.makePlutusV3VM()

        val v1CEK = PlutusV3.compile { targetToCompactBits }.program
        val v2CEK = PlutusV3.compile { targetToCompactBitsV2 }.program

        val testCases = Seq(
          ("zero", BigInt(0)),
          ("small: 0x12", BigInt(0x12)),
          ("2-byte: 0x1234", BigInt(0x1234)),
          ("3-byte: 0x123456", BigInt(0x123456)),
          ("4-byte: 0x12345600", BigInt(0x12345600L)),
          (
            "max target (PowLimit)",
            BigInt("00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff", 16)
          ),
          (
            "large: 0x1234560000...0",
            BigInt(
              "1234560000000000000000000000000000000000000000000000000000000000",
              16
            )
          ),
          (
            "real block 865494 target",
            compactBitsToTarget(hex"17030ecd".reverse)
          )
        )

        println(
          f"${"Test case"}%-30s | ${"V1 (recursive)"}%-30s | ${"V2 (findFirstSetBit)"}%-30s | ${"Savings"}%-20s"
        )
        println("-" * 115)

        for (name, target) <- testCases do
            val arg = target
            val r1 = (v1CEK $ arg.asTerm).term.evaluateDebug
            val r2 = (v2CEK $ arg.asTerm).term.evaluateDebug

            (r1, r2) match
                case (Result.Success(t1, b1, _, _), Result.Success(t2, b2, _, _)) =>
                    // Verify both produce the same result
                    assert(t1 == t2, s"$name: V1 and V2 produced different results")
                    val cpuSaved = b1.steps - b2.steps
                    val memSaved = b1.memory - b2.memory
                    val cpuPct = if b1.steps > 0 then cpuSaved * 100.0 / b1.steps else 0.0
                    val memPct = if b1.memory > 0 then memSaved * 100.0 / b1.memory else 0.0
                    println(
                      f"$name%-30s | cpu=${b1.steps}%10d mem=${b1.memory}%8d | cpu=${b2.steps}%10d mem=${b2.memory}%8d | cpu=$cpuPct%+.1f%% mem=$memPct%+.1f%%"
                    )
                case (Result.Failure(e1, _, _, _), _) =>
                    fail(s"$name: V1 failed: $e1")
                case (_, Result.Failure(e2, _, _, _)) =>
                    fail(s"$name: V2 failed: $e2")
    }
}
