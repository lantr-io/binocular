package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as POption}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/** Pins the Scala mirror of `lib/bifrost/types/config.ak::ConfigDatum` to the on-chain positional
  * encoding: 20 Constr-0 fields, `update_auth` (18) as Aiken `Option` (Some = Constr 0 [v], None =
  * Constr 1 []) and the fBTC mint-checker hash at index 19.
  */
class ConfigDatumEncodingTest extends AnyFunSuite {

    test("updateAuth Some/None encode as Constr 0 [v] / Constr 1 [] (Aiken Option)") {
        val some: POption[AuthorizationMethod] =
            POption.Some(AuthorizationMethod.CardanoSignature(ByteString.fromHex("aa")))
        val none: POption[AuthorizationMethod] = POption.None
        assert(some.toData match {
            case Data.Constr(0, fields) => fields.asScala.size == 1
            case _                      => false
        })
        assert(none.toData == Data.Constr(1, PList()))
    }

    test("ConfigDatum has 20 positional fields; checker hash is field 19") {
        val checker = ByteString.fromHex("ee" * 28)
        val d = ConfigDatum(
          bridgedTokenPolicyId = ByteString.fromHex("aa" * 28),
          bridgedTokenAssetName = ByteString.fromString("fBTC"),
          sourceChainMerkleTreePolicyId = ByteString.empty,
          sourceChainMerkleTreeAssetName = ByteString.empty,
          blockHeaderMerkleTreePolicyId = ByteString.empty,
          blockHeaderMerkleTreeAssetName = ByteString.empty,
          completedPegInsMerkleTreePolicyId = ByteString.empty,
          completedPegInsMerkleTreeAssetName = ByteString.empty,
          completedPegOutsMerkleTreePolicyId = ByteString.empty,
          completedPegOutsMerkleTreeAssetName = ByteString.empty,
          pegInWithdrawScriptHash = ByteString.empty,
          pegOutWithdrawScriptHash = ByteString.empty,
          pegInCloseVerifierScriptHash = ByteString.empty,
          legitTmAndPegOutProducedVerifierScriptHash = ByteString.empty,
          legitTmAndPegOutNotProducedVerifierScriptHash = ByteString.empty,
          treasuryNftPolicyId = ByteString.empty,
          treasuryNftAssetName = ByteString.empty,
          minStake = BigInt(0),
          updateAuth = POption.None,
          bridgedTokenMintCheckerScriptHash = checker
        )
        d.toData match {
            case Data.Constr(0, fields) =>
                val fs = fields.asScala.toIndexedSeq
                assert(fs.size == 20)
                assert(fs(19) == Data.B(checker))
            case other => fail(s"expected Constr 0, got $other")
        }
    }
}
