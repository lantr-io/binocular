package binocular

import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.{List as PList, Option as POption}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

/** Pins the Scala mirror of `lib/bifrost/types/config.ak::ConfigDatum` to the on-chain positional
  * encoding: 11 Constr-0 fields, `update_auth` (10) as Aiken `Option` (Some = Constr 0 [v], None =
  * Constr 1 []).
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

    test("ConfigDatum has 11 positional fields; update_auth is field 10") {
        val d = ConfigDatum(
          bridgedTokenPolicyId = ByteString.fromHex("aa" * 28),
          bridgedTokenAssetName = ByteString.fromString("fSAT"),
          completedPegInsMerkleTreePolicyId = ByteString.empty,
          completedPegOutsMerkleTreePolicyId = ByteString.empty,
          pegInWithdrawScriptHash = ByteString.empty,
          pegOutWithdrawScriptHash = ByteString.empty,
          pegInCloseVerifierScriptHash = ByteString.empty,
          legitTmAndPegOutProducedVerifierScriptHash = ByteString.empty,
          legitTmAndPegOutNotProducedVerifierScriptHash = ByteString.empty,
          minStake = BigInt(0),
          updateAuth = POption.None
        )
        d.toData match {
            case Data.Constr(0, fields) =>
                val fs = fields.asScala.toIndexedSeq
                assert(fs.size == 11)
                assert(fs(10) == Data.Constr(1, PList()))
            case other => fail(s"expected Constr 0, got $other")
        }
    }
}
