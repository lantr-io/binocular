package binocular

import binocular.attack.RogueMiner
import binocular.bitcoin.BitcoinHelpers.*
import binocular.oracle.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.uplc.builtin.ByteString

class RogueMinerTest extends AnyFunSuite {

    // Regtest powLimit (0x7fff...) makes the nonce search trivial, while still
    // genuinely exercising PoW. Same code path testnet4 uses, just easy to mine.
    private def regtestParams: BitcoinValidatorParams = {
        val dummyRef = TxOutRef(
          TxId(ByteString.fromHex("00" * 32)),
          0
        )
        val owner = PubKeyHash(ByteString.fromHex("00" * 28))
        BitcoinValidatorParams.makeRegtest(dummyRef, owner)
    }

    // A standalone parent context to mine on top of: difficulty = regtest powLimit,
    // 11 identical timestamps so MTP is well-defined, arbitrary parent hash.
    private def parentCtx(params: BitcoinValidatorParams): TraversalCtx = {
        val ts: BigInt = 1_700_000_000
        TraversalCtx(
          timestamps = PList.from((0 until 11).map(_ => ts).toList),
          height = BigInt(500),
          currentBits = targetToCompactByteString(params.powLimit),
          prevDiffAdjTimestamp = ts,
          lastBlockHash = ByteString.fromHex("ab" * 32)
        )
    }

    test("mined rogue block passes validateBlock and commits to a fake merkle root") {
        val params = regtestParams
        val ctx = parentCtx(params)
        val now: BigInt = ctx.timestamps.head + 2000 // inside +2h window, > parent+1200

        val mined = RogueMiner.mineBlock(ctx, now, blockSpacing = 1200, params = params)

        // It must validate against the on-chain rule set (no exception thrown).
        val (summary, newCtx, _) = BitcoinValidator.validateBlock(mined.header, ctx, now, params)
        assert(summary.hash == mined.summary.hash)
        assert(newCtx.height == ctx.height + 1)

        // Header links to the parent and carries an 80-byte body.
        assert(mined.header.bytes.length == BigInt(80))
        assert(mined.header.prevBlockHash == ctx.lastBlockHash)

        // The merkle root is a *real* tree root over fabricated tx hashes — not zero,
        // and not equal to any single tx hash (i.e. a genuine commitment).
        assert(mined.header.merkleRoot != ByteString.fromArray(new Array[Byte](32)))
    }

    test("mineBlock chains: a second block validates on top of the first") {
        val params = regtestParams
        val ctx0 = parentCtx(params)
        val now: BigInt = ctx0.timestamps.head + 2000
        val b1 = RogueMiner.mineBlock(ctx0, now, 1200, params)
        val b2 = RogueMiner.mineBlock(b1.ctxAfter, now + 1300, 1200, params)
        // b2 must validate against the ctx produced by b1.
        BitcoinValidator.validateBlock(b2.header, b1.ctxAfter, now + 1300, params)
        assert(b2.header.prevBlockHash == b1.summary.hash)
    }
}
