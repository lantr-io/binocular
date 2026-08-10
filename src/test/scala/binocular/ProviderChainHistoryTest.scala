package binocular

import binocular.watchtower.ProviderChainHistory

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{AssetName, Coin, Credential, DataHash, DatumOption, ScriptHash, TransactionHash, TransactionInput, TransactionOutput, Value}
import scalus.uplc.builtin.{ByteString, Data}

/** Unit tests for the ledger-output-to-[[ChainOutput]] mapping behind [[ProviderChainHistory]], the
  * history source that rides on the scalus `BlockfrostProvider` instead of a second HTTP client.
  *
  * The three-way datum distinction is the load-bearing part: reconstruction treats "no datum" as
  * provably-not-a-TM-record, "inline" as readable, and "present but unreadable" (a bare datum hash)
  * as a reason to warn — collapsing any two of those changes what a reconstruction may skip.
  */
class ProviderChainHistoryTest extends AnyFunSuite {

    private val addr =
        Address(Network.Testnet, Credential.ScriptHash(ScriptHash.fromHex("aa" * 28)))

    private val input =
        TransactionInput(TransactionHash.fromHex("bb" * 32), 3)

    private def out(value: Value, datumOption: Option[DatumOption]): TransactionOutput =
        TransactionOutput.Babbage(addr, value, datumOption = datumOption, scriptRef = None)

    test("an inline datum maps to inlineDatum, with no unresolved reason") {
        val datum = Data.I(BigInt(42))
        val co = ProviderChainHistory.toChainOutput(
          input,
          out(Value(Coin(2_000_000L)), Some(DatumOption.Inline(datum)))
        )
        assert(co.txHash == ByteString.fromHex("bb" * 32))
        assert(co.outputIndex == 3L)
        assert(co.inlineDatum.contains(datum))
        assert(co.unresolvedDatum.isEmpty)
        assert(co.ref == s"${"bb" * 32}#3")
    }

    test("a bare datum hash maps to unresolvedDatum: present but unreadable") {
        val co = ProviderChainHistory.toChainOutput(
          input,
          out(Value(Coin(2_000_000L)), Some(DatumOption.Hash(DataHash.fromHex("cc" * 32))))
        )
        assert(co.inlineDatum.isEmpty)
        assert(co.unresolvedDatum.exists(_.contains("cc" * 32)))
    }

    test("no datum at all maps to neither: provably not a TM record") {
        val co = ProviderChainHistory.toChainOutput(input, out(Value(Coin(2_000_000L)), None))
        assert(co.inlineDatum.isEmpty)
        assert(co.unresolvedDatum.isEmpty)
    }

    test("assets map to Blockfrost units, so quantityOf answers like the HTTP source") {
        val policy = ScriptHash.fromHex("dd" * 28)
        val name = AssetName(ByteString.fromString("CPO"))
        val value = Value(Coin(2_000_000L)) + Value.asset(policy, name, 1L)
        val co = ProviderChainHistory.toChainOutput(input, out(value, None))
        assert(co.assets("lovelace") == BigInt(2_000_000))
        assert(co.quantityOf("dd" * 28, name.bytes.toHex) == BigInt(1))
        assert(co.quantityOf("ee" * 28, name.bytes.toHex) == BigInt(0))
    }
}
