package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{AssetName, Coin, Credential, DatumOption, ScriptHash, TransactionHash, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

import java.nio.file.Files

/** Tests for the sweeper's PURE decisions: which peg-out requests a sweep submits a Complete for,
  * and whether the derived scripts match the deployed Config.
  *
  * The submission path itself needs a chain, so it is not exercised here; what IS exercised is
  * every rule that decides whether a transaction gets built at all. Getting those wrong is what
  * would make the watchtower either burn fees on doomed transactions or quietly leave paid requests
  * behind.
  */
class PorSweeperTest extends AnyFunSuite {

    private val network = Network.Testnet
    private val pegOutHash = ScriptHash.fromHex("9a" * 28)
    private val bridgedTokenPolicy = ScriptHash.fromHex("a1" * 28)
    private val bridgedTokenAsset = AssetName(ByteString.fromString("fSAT"))
    private val configNftPolicy = ScriptHash.fromHex("c0" * 28)
    private val destSpk = ByteString.fromHex("0014" + "ab" * 20)

    private val blueprint = BifrostBlueprint.packaged

    /** A Context whose scripts are the real derived ones, so `verifyAgainstConfig` is meaningful.
      */
    private val pegOut = PegOutContract(
      blueprint,
      ByteString.fromArray(configNftPolicy.bytes),
      ByteString.fromString("BIFCFG")
    )
    private val bridgedToken = BridgedTokenContract(
      blueprint,
      ByteString.fromArray(configNftPolicy.bytes),
      ByteString.fromString("BIFCFG")
    )

    private val ctx = PorSweeper.Context(
      network = network,
      tmAddress = Address(network, Credential.ScriptHash(ScriptHash.fromHex("11" * 28))),
      pegOutScript = pegOut.script,
      pegOutAddress = pegOut.address(network),
      bridgedTokenScript = bridgedToken.script,
      bridgedTokenPolicy = bridgedToken.policyId,
      bridgedTokenAsset = bridgedTokenAsset,
      resolveScriptRefs = () => PegOutCompleteTx.ScriptRefs(None, None)
    )

    private def porDatum(fee: Long, dest: ByteString = destSpk): Data = PegOutDatum(
      ownerAuth = AuthorizationMethod.CardanoSignature(ByteString.fromHex("d0" * 28)),
      sourceChainDestinationAddress = dest,
      perPegoutFee = BigInt(fee),
      created = BigInt(1_700_000_000_000L)
    ).toData

    /** A peg-out request UTxO. `datum = None` and `locked = 0` model the junk anyone can pay to the
      * permissionlessly-payable peg-out address.
      */
    private def porUtxo(
        seed: Int,
        index: Int,
        locked: Long,
        datum: Option[Data]
    ): Utxo = {
        val value =
            if locked > 0 then
                Value(Coin(2_000_000L)) +
                    Value.asset(ctx.bridgedTokenPolicy, bridgedTokenAsset, locked)
            else Value(Coin(2_000_000L))
        Utxo(
          TransactionInput(TransactionHash.fromHex(f"$seed%02x" * 32), index),
          TransactionOutput.Babbage(
            ctx.pegOutAddress,
            value,
            datumOption = datum.map(DatumOption.Inline(_)),
            scriptRef = None
          )
        )
    }

    private def entryFor(u: Utxo, dest: ByteString, net: Long): (ByteString, ByteString) = {
        val txHash = ByteString.fromArray(u.input.transactionId.bytes)
        CpoTrieMirror.porId(txHash, u.input.index.toLong) ->
            CompletedPegOutsTrie.trieValue(PegOutEntry(dest, BigInt(net)))
    }

    private def mirror(entries: Seq[(ByteString, ByteString)]): CpoTrieMirror =
        CpoTrieMirror.fromEntries(entries).fold(e => fail(e), identity)

    // --- candidate selection --------------------------------------------------------------------

    test("a request the trie records is completable") {
        val u = porUtxo(0x51, 0, 100_000L, Some(porDatum(1_000L)))
        val m = mirror(Seq(entryFor(u, destSpk, 99_000L)))
        val (ready, skipped) = PorSweeper.candidates(Seq(u), ctx, m)
        assert(ready.map(_.ref) == Seq(s"${u.input.transactionId.toHex}#0"))
        assert(ready.head.locked == 100_000L)
        assert(skipped.isEmpty)
    }

    test("an OPEN request is ignored silently, never reported") {
        // Not-yet-paid is the normal state of a request; reporting it would make the log useless.
        val u = porUtxo(0x52, 0, 100_000L, Some(porDatum(1_000L)))
        val (ready, skipped) = PorSweeper.candidates(Seq(u), ctx, CpoTrieMirror.empty)
        assert(ready.isEmpty)
        assert(skipped.isEmpty)
    }

    test("a request whose trie value disagrees with its own binding is REPORTED, not attempted") {
        // `peg-out.ak` rebuilds the value from THIS UTxO, so submitting would burn a fee on a
        // script the ledger rejects.
        val u = porUtxo(0x53, 0, 100_000L, Some(porDatum(1_000L)))
        val m = mirror(Seq(entryFor(u, destSpk, 98_000L))) // paid amount disagrees with the fee
        val (ready, skipped) = PorSweeper.candidates(Seq(u), ctx, m)
        assert(ready.isEmpty)
        assert(skipped.size == 1)
        assert(skipped.head.reason.contains("but this request binds"))
    }

    test("a request paid to a different destination is REPORTED") {
        val u = porUtxo(0x54, 0, 100_000L, Some(porDatum(1_000L)))
        val m = mirror(Seq(entryFor(u, ByteString.fromHex("0014" + "cd" * 20), 99_000L)))
        val (ready, skipped) = PorSweeper.candidates(Seq(u), ctx, m)
        assert(ready.isEmpty)
        assert(skipped.size == 1)
    }

    test("junk at the peg-out address is neither completed nor reported") {
        val noFbtc = porUtxo(0x55, 0, 0L, Some(porDatum(1_000L)))
        val noDatum = porUtxo(0x56, 0, 100_000L, None)
        val notAPor = porUtxo(0x57, 0, 100_000L, Some(Data.I(BigInt(1))))
        val (ready, skipped) =
            PorSweeper.candidates(Seq(noFbtc, noDatum, notAPor), ctx, CpoTrieMirror.empty)
        assert(ready.isEmpty)
        assert(skipped.isEmpty)
    }

    test("several paid requests are all selected, and the index is part of the POR id") {
        // Two outputs of the SAME transaction: only the output index distinguishes them.
        val a = porUtxo(0x58, 0, 100_000L, Some(porDatum(1_000L)))
        val b = porUtxo(0x58, 1, 100_000L, Some(porDatum(1_000L)))
        val m = mirror(Seq(entryFor(a, destSpk, 99_000L), entryFor(b, destSpk, 99_000L)))
        assert(m.size == 2)
        val (ready, _) = PorSweeper.candidates(Seq(a, b), ctx, m)
        assert(ready.size == 2)
    }

    test("historicalPor computes net = locked - pinned fee, floored at zero") {
        val u = porUtxo(0x59, 0, 500L, Some(porDatum(1_000L)))
        val por = PorSweeper.historicalPor(u, ctx).getOrElse(fail("expected a request"))
        assert(por.netSat == 0)
    }

    // --- config agreement -----------------------------------------------------------------------

    // The rev-5.4 raw layout: 0 update_auth (None), 1 bridged_token_policy, 2-5 inert, 6 the
    // peg_out script hash. The asset name is the [CFG-1] constant, so it is NOT in the datum.
    private def configUtxo(pegOutField: ScriptHash, policy: ScriptHash): Utxo = {
        val datum: Data = Data.Constr(
          0,
          PList.from(
            List[Data](
              Data.Constr(1, PList()), // update_auth = None
              Data.B(ByteString.fromArray(policy.bytes)),
              Data.B(ByteString.empty),
              Data.B(ByteString.empty),
              Data.B(ByteString.empty),
              Data.B(ByteString.empty),
              Data.B(ByteString.fromArray(pegOutField.bytes))
            )
          )
        )
        Utxo(
          TransactionInput(TransactionHash.fromHex("c0" * 32), 0),
          TransactionOutput.Babbage(
            Address(network, Credential.ScriptHash(configNftPolicy)),
            Value(Coin(2_000_000L)),
            datumOption = Some(DatumOption.Inline(datum)),
            scriptRef = None
          )
        )
    }

    test("a Config publishing our scripts lets the sweeper run") {
        val u = configUtxo(pegOut.policyId, bridgedToken.policyId)
        assert(PorSweeper.verifyAgainstConfig(u, ctx) == Right(()))
    }

    test("a Config still publishing the pre-migration peg-out hash blocks sweeping") {
        // The state between deploying the new peg_out and running `update-config
        // --peg-out-withdraw-hash`. Confirming keeps working; only cleanup waits.
        val u = configUtxo(pegOutHash, bridgedToken.policyId)
        val err = PorSweeper.verifyAgainstConfig(u, ctx).swap.getOrElse(fail("expected a mismatch"))
        assert(err.contains("update-config --peg-out-withdraw-hash"))
    }

    test("a Config publishing a different bridged token blocks sweeping") {
        val u = configUtxo(pegOut.policyId, bridgedTokenPolicy)
        assert(PorSweeper.verifyAgainstConfig(u, ctx).isLeft)
    }

    test("a Config datum that is not a Constr 0 is reported, not thrown") {
        val u = Utxo(
          TransactionInput(TransactionHash.fromHex("c1" * 32), 0),
          TransactionOutput.Babbage(
            Address(network, Credential.ScriptHash(configNftPolicy)),
            Value(Coin(2_000_000L)),
            datumOption = Some(DatumOption.Inline(Data.I(BigInt(1)))),
            scriptRef = None
          )
        )
        assert(PorSweeper.verifyAgainstConfig(u, ctx).isLeft)
    }

    // --- pending hint persistence ---------------------------------------------------------------

    /** The queue must survive a restart. Without it, a process that dies between a Confirm and the
      * next catch-up loses the hints that explain the new on-chain root, and the sweeper falls back
      * to a full two-address reconstruction — or, when the backend is unavailable, to no catch-up
      * at all.
      */
    private def tempDir() = Files.createTempDirectory("por-sweeper-test")

    private def pendingTm(seed: Int, entries: Int): PorSweeper.PendingTm = PorSweeper.PendingTm(
      btcTxidDisplay = f"$seed%02x" * 32,
      attestedRoot = ByteString.fromArray(Array.fill[Byte](32)((seed + 1).toByte)),
      entries = (0 until entries).map { i =>
          (
            ByteString.fromArray(Array.fill[Byte](32)((seed + i).toByte)),
            CompletedPegOutsTrie.trieValue(PegOutEntry(destSpk, BigInt(1000 + i)))
          )
      }
    )

    test("the pending hint queue round-trips through the state directory") {
        val dir = tempDir()
        val queue = Seq(pendingTm(0x10, 2), pendingTm(0x20, 0))
        assert(PorSweeper.PendingTm.save(dir, queue) == Right(()))
        assert(PorSweeper.PendingTm.load(dir) == Right(queue))
    }

    test("an absent pending file is an empty queue, not an error") {
        assert(PorSweeper.PendingTm.load(tempDir()) == Right(Seq.empty))
    }

    test("a pending file from an unknown version is ignored rather than trusted") {
        val dir = tempDir()
        PorSweeper.PendingTm.save(dir, Seq(pendingTm(0x30, 1)))
        val file = PorSweeper.PendingTm.file(dir)
        val json = ujson.read(Files.readString(file))
        json("version") = ujson.Num(PorSweeper.PendingTm.Version + 1)
        Files.write(file, ujson.write(json).getBytes("UTF-8"))
        assert(PorSweeper.PendingTm.load(dir).isLeft)
    }

    test("an unreadable pending file is reported, not thrown") {
        // Hints are unverified either way — each batch is checked against its TM's attested root
        // before it is folded in — so a bad queue costs a reconstruction, never correctness.
        val dir = tempDir()
        Files.write(PorSweeper.PendingTm.file(dir), "not json".getBytes("UTF-8"))
        assert(PorSweeper.PendingTm.load(dir).isLeft)
    }

    test("saving an empty queue clears a previously persisted one") {
        val dir = tempDir()
        PorSweeper.PendingTm.save(dir, Seq(pendingTm(0x40, 1)))
        PorSweeper.PendingTm.save(dir, Seq.empty)
        assert(PorSweeper.PendingTm.load(dir) == Right(Seq.empty))
    }
}
