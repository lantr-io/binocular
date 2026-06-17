package binocular

import binocular.attack.RoguePlanner
import binocular.bitcoin.*
import binocular.bitcoin.BitcoinHelpers.*
import binocular.oracle.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString

class RoguePlannerTest extends AnyFunSuite {

    private def params = BitcoinValidatorParams.makeRegtest(
      scalus.cardano.onchain.plutus.v3
          .TxOutRef(scalus.cardano.onchain.plutus.v3.TxId(ByteString.fromHex("00" * 32)), 0),
      scalus.cardano.onchain.plutus.v1.PubKeyHash(ByteString.fromHex("00" * 28))
    )

    // Fresh oracle state with an empty fork tree at the confirmed tip.
    private def freshState(ts: BigInt): ChainState =
        ChainState(
          confirmedBlocksRoot = ByteString.fromHex("00" * 32),
          ctx = TraversalCtx(
            timestamps = PList.from((0 until 11).map(_ => ts).toList),
            height = BigInt(100),
            currentBits = targetToCompactByteString(params.powLimit),
            prevDiffAdjTimestamp = ts,
            lastBlockHash = ByteString.fromHex("ab" * 32)
          ),
          forkTree = ForkTree.End
        )

    // rpc is unused by RoguePlanner; a null is fine since planUpdate never calls it.
    private val noRpc: BitcoinRpc = null

    test("first cycle mines a sprint of valid rogue blocks off the confirmed tip") {
        val ts: BigInt = 1_700_000_000
        val state = freshState(ts)
        val planner = new RoguePlanner(
          parent = "0",
          rogueSprint = 3,
          blockSpacing = 1200,
          nowProvider = () => ts
        )
        val plan = planner.planUpdate(noRpc, state, OffChainMPF.empty, validityTime = ts, params)

        assert(plan.headers.length == BigInt(3))
        assert(plan.parentPath == PList.Nil) // attaches at confirmed tip

        // Every mined header must validate as a chain off the confirmed tip.
        var ctx = state.ctx
        plan.headers.toScalaList.foreach { h =>
            val (_, next, _) = BitcoinValidator.validateBlock(h, ctx, ts + 8000, params)
            ctx = next
        }
        assert(ctx.height == BigInt(103))
    }
}
