package binocular

import binocular.oracle.WalletConfig
import binocular.watchtower.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.uplc.builtin.ByteString

class PegOutRequestTxTest extends AnyFunSuite {
    private val chainInfo = CardanoInfo.preprod
    private val sponsor = WalletConfig("abandon " * 11 + "about").createHdAccount().toOption.get
    private val address = sponsor.baseAddress(Network.Testnet)
    private val pegOut =
        Address(Network.Testnet, Credential.ScriptHash(ScriptHash.fromHex("aa" * 28)))
    private val policy = ScriptHash.fromHex("bb" * 28)
    private val asset = AssetName(ByteString.fromString("fSAT"))
    private val extra = AssetName(ByteString.fromString("unrelated"))
    private val datum = PegOutDatum(
      AuthorizationMethod.CardanoSignature(ByteString.fromArray(sponsor.paymentKeyHash.bytes)),
      ByteString.fromHex("5120" + "11" * 32),
      1000,
      1700000000000L
    )
    private def input(n: Int) = TransactionInput(TransactionHash.fromHex(f"$n%02x" * 32), 0)
    private def funding(n: Int, amount: Long, unrelated: Boolean = false) =
        input(n) -> TransactionOutput.Babbage(
          address,
          Value.lovelace(10000000) + Value.asset(policy, asset, amount) +
              (if unrelated then Value.asset(policy, extra, 10) else Value.zero)
        )
    private def build(utxos: Utxos, excluded: Set[TransactionInput] = Set.empty) =
        PegOutRequestTx.build(
          chainInfo,
          sponsor,
          utxos,
          excluded,
          pegOut,
          policy,
          asset,
          30000,
          datum
        )

    test("returns a signed transaction with exact fBTC, minimum ADA, datum and no extra tokens") {
        val tx = build(Map(funding(1, 50000, true)))
        val out = tx.body.value.outputs.head
        assert(out.value.address == pegOut)
        assert(out.value.value.asset(policy, asset) == 30000)
        assert(out.value.value.assets == Value.asset(policy, asset, 30000).assets)
        assert(out.value.inlineDatum.get.to[PegOutDatum] == datum)
        assert(
          out.value.value.coin == MinCoinSizedTransactionOutput.computeMinAda(
            out,
            chainInfo.protocolParams
          )
        )
        assert(tx.witnessSet.vkeyWitnesses.toSeq.nonEmpty)
    }

    test("library coin selection combines fragmented fBTC inputs") {
        val tx = build(Map(funding(1, 20000), funding(2, 10000)))
        assert(tx.body.value.inputs.toSet == Set(input(1), input(2)))
    }

    test("reference-script outpoints never enter coin selection") {
        val tx = build(Map(funding(1, 50000), funding(2, 30000)), Set(input(1)))
        assert(!tx.body.value.inputs.toSet.contains(input(1)))
        intercept[Exception] { build(Map(funding(1, 50000)), Set(input(1))) }
    }

    test("shared funding filter rejects excluded, reference-script and foreign outputs") {
        val (ref, output) = funding(1, 50000)
        val scriptOutput =
            output.copy(scriptRef = Some(ScriptRef(Script.PlutusV3(ByteString.fromHex("01")))))
        assert(CardanoFunding.eligible(Map(ref -> scriptOutput), address, Set.empty).isEmpty)
        assert(
          CardanoFunding
              .eligible(Map(ref -> output.copy(address = pegOut)), address, Set.empty)
              .isEmpty
        )
        assert(CardanoFunding.eligible(Map(ref -> output), address, Set(ref)).isEmpty)
        assert(CardanoFunding.eligible(Map(ref -> output), address, Set.empty).size == 1)
    }

    test("rejects an amount that cannot pay the pinned fee") {
        intercept[IllegalArgumentException] {
            PegOutRequestTx.build(
              chainInfo,
              sponsor,
              Map(funding(1, 50000)),
              Set.empty,
              pegOut,
              policy,
              asset,
              1000,
              datum
            )
        }
    }
}
