package binocular

import binocular.cli.CommandHelpers

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ScriptHash, TransactionHash, TransactionInput}

/** Pins the pure parsing of Blockfrost `/addresses/{addr}/utxos` JSON into the
  * `(reference_script_hash -> outpoint)` pairs used to discover CIP-33 reference-script UTxOs by
  * script hash (so the bridge no longer needs the outpoints recorded in config).
  */
class RefScriptDiscoveryTest extends AnyFunSuite {

    private val hashA = "a" * 56 // 28-byte script hash
    private val hashB = "b" * 56
    private val tx1 = "1" * 64 // 32-byte tx hash
    private val tx2 = "2" * 64

    private def utxo(txHash: String, idx: Int, refHash: String | Null): ujson.Value = {
        val base = ujson.Obj(
          "tx_hash" -> txHash,
          "output_index" -> idx
        )
        base("reference_script_hash") = if refHash == null then ujson.Null else ujson.Str(refHash)
        base
    }

    test("keeps ref-script UTxOs as (scriptHash -> outpoint), dropping non-ref UTxOs") {
        val items = Seq(
          utxo(tx1, 0, hashA),
          utxo(tx2, 3, hashB),
          utxo(tx1, 1, null) // plain wallet UTxO, no reference script
        )

        val pairs = CommandHelpers.parseRefScriptOutpoints(items)

        assert(
          pairs.toSet == Set(
            ScriptHash.fromHex(hashA) -> TransactionInput(TransactionHash.fromHex(tx1), 0),
            ScriptHash.fromHex(hashB) -> TransactionInput(TransactionHash.fromHex(tx2), 3)
          )
        )
    }

    test("preserves duplicate UTxOs carrying the same script hash") {
        val items = Seq(utxo(tx1, 0, hashA), utxo(tx2, 0, hashA))

        val outpoints = CommandHelpers.parseRefScriptOutpoints(items).map(_._2).toSet

        assert(
          outpoints == Set(
            TransactionInput(TransactionHash.fromHex(tx1), 0),
            TransactionInput(TransactionHash.fromHex(tx2), 0)
          )
        )
    }
}
