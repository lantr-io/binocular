package binocular.watchtower

import scalus.cardano.onchain.plutus.prelude.{List as ScalusList, *}
import scalus.cardano.onchain.plutus.v3.*
import scalus.compiler.{Compile, Options}
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.toData

/** A one-shot minting policy: a mint is allowed only if the transaction consumes the parameterized
  * output reference. Because an outpoint can be spent exactly once, the policy can ever mint at
  * most once — making its token a true singleton (NFT) that cannot be re-minted or forged. Used to
  * authenticate the TM-control UTxO ([[TmControlDatum]]). Parameterized by the one-shot outpoint.
  */
@Compile
object OneShotMint {

    def validate(oneShotRef: Data, scData: Data): Unit = {
        val sc = unConstrData(scData).snd
        val txInfo = sc.head.to[TxInfo]
        val scriptInfo = unConstrData(sc.tail.tail.head)
        if scriptInfo.fst == BigInt(0) then {
            // MintingScript: require the one-shot outpoint to be among the spent inputs.
            def consumed(inputs: ScalusList[TxInInfo]): Boolean =
                inputs match
                    case ScalusList.Nil => false
                    case ScalusList.Cons(i, tail) =>
                        if equalsData(i.outRef.toData, oneShotRef) then true else consumed(tail)
            require(consumed(txInfo.inputs), "one-shot input not consumed")
        } else fail("OneShotMint: not a minting script")
    }
}

object OneShotMintContract {
    given opts: Options = Options.release

    lazy val parameterized: PlutusV3[Data => (Data => Unit)] =
        PlutusV3.compile((oneShotRef: Data) =>
            (scData: Data) => OneShotMint.validate(oneShotRef, scData)
        )

    /** The one-shot policy that can mint exactly once, when `oneShotRef` is consumed. */
    def contract(oneShotRef: TxOutRef): PlutusV3[Data => Unit] =
        parameterized.apply(oneShotRef.toData)
}
