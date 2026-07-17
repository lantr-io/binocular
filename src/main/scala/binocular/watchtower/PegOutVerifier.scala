package binocular.watchtower

import binocular.bitcoin.*
import binocular.blueprint.BinocularBlueprint
import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.cardano.ledger.Script

import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.*
import scalus.compiler.{Compile, Options}
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*

/** The `legit_treasury_movement_and_peg_out_produced_verifier` — config[7].
  *
  * `peg_out.ak::CompletePegOut` mandatorily delegates to this script via
  * `stake_validator.validate_withdraw`: it finds the withdrawal whose credential is this script's
  * hash, runs this validator, and reads the withdrawal's redeemer. `peg_out.ak` then cross-checks
  * those redeemer fields equal the PegOutDatum (treasury utxo id, destination address), the
  * `peg_out_utxo_id`, the locked-fBTC quantity, and the raw TM tx. So the redeemer is the trust
  * anchor — THIS validator must independently prove, from the raw TM bytes, that the TM both
  *   1. **spends** the treasury outpoint named in the redeemer, and
  *   2. **produces** an output paying the peg-out destination scriptPubKey the peg-out amount.
  *
  * The redeemer is a bare Plutus `List<Data>` (built by us in the peg-out-complete tx and read by
  * both `peg_out.ak` via `un_list_data` and here):
  * {{{
  *   [ source_chain_treasury_utxo_id : B
  *   , source_chain_destination_address : B   // raw Bitcoin scriptPubKey
  *   , peg_out_utxo_id : B
  *   , peg_out_amount : I                      // = locked fBTC qty
  *   , treasury_movement_raw_tx : B
  *   , .. ]
  * }}}
  *
  * Note (demo, [[per_pegout_fee]] = 0): the on-chain check is the clean equality
  * `btc_output_value == peg_out_amount`. heimdall must build the TM with `per_pegout_fee = 0` so
  * the paid satoshis equal the fBTC quantity exactly; a non-zero fee would require this verifier to
  * subtract a fee it cannot trust. The TM's *Bitcoin confirmation* is proven by `peg_out.ak`
  * against the Binocular oracle — not re-checked here.
  *
  * The raw TM tx is walked exactly ONCE (inputs then outputs, in a single forward pass) rather than
  * reusing [[TreasuryMovementValidator]]'s `allInputOutpoints` + `allOutputs` — those would walk
  * the input region twice (the second time via `skipTxIns` inside `allOutputs`) and allocate two
  * intermediate lists. [[scanTm]] streams over the bytes, short-circuits, and allocates nothing.
  */
@Compile
object PegOutProducedVerifier {

    def validate(scData: Data): Unit = {
        val ctx = scData.to[ScriptContext]
        ctx.scriptInfo match
            case ScriptInfo.RewardingScript(_) =>
                val items = unListData(ctx.redeemer)
                val treasuryUtxoId = unBData(items.head)
                val afterTreasury = items.tail
                val destinationAddress = unBData(afterTreasury.head)
                // index 2 (peg_out_utxo_id) is not needed for the produced check — skip it.
                val afterPegOutId = afterTreasury.tail.tail
                val pegOutAmount = unIData(afterPegOutId.head)
                val rawTx = unBData(afterPegOutId.tail.head)

                require(
                  scanTm(rawTx, treasuryUtxoId, destinationAddress, pegOutAmount),
                  "TM must spend the treasury outpoint AND produce an output paying the peg-out " +
                      "destination the peg-out amount"
                )
            case _ => fail("peg-out produced verifier: only the rewarding purpose is valid")
    }

    /** Single forward pass over a raw Bitcoin tx. Returns true iff the tx (1) spends
      * `treasuryUtxoId` as one of its inputs AND (2) produces an output paying `destinationSpk`
      * exactly `pegOutAmount` satoshis. Input-region offset advancement mirrors
      * [[TreasuryMovementValidator.allInputOutpoints]]; output parsing mirrors `allOutputs` — kept
      * byte-for-byte consistent so the two paths agree.
      */
    def scanTm(
        rawTx: ByteString,
        treasuryUtxoId: ByteString,
        destinationSpk: ByteString,
        pegOutAmount: BigInt
    ): Boolean = {
        val txInsStart = if BitcoinHelpers.isWitnessTransaction(rawTx) then BigInt(6) else BigInt(4)
        val insNumAndOffset = BitcoinHelpers.readVarInt(rawTx, txInsStart)

        // Walk inputs once: detect the treasury outpoint, and find where the output section starts.
        def scanInputs(
            remaining: BigInt,
            offset: BigInt,
            foundTreasury: Boolean
        ): (Boolean, BigInt) =
            if remaining == BigInt(0) then (foundTreasury, offset)
            else
                val outpoint = rawTx.slice(offset, 36)
                val lenAndAfter = BitcoinHelpers.readVarInt(rawTx, offset + 36)
                val scriptLen = lenAndAfter._1
                val afterVarInt = lenAndAfter._2
                val nextOffset = afterVarInt + scriptLen + 4 // + 4-byte sequence
                scanInputs(
                  remaining - 1,
                  nextOffset,
                  foundTreasury || outpoint == treasuryUtxoId
                )

        val inputsResult = scanInputs(insNumAndOffset._1, insNumAndOffset._2, false)
        val treasurySpent = inputsResult._1
        val afterIns = inputsResult._2

        // Walk outputs once, continuing from where the inputs ended.
        val outsNumAndOffset = BitcoinHelpers.readVarInt(rawTx, afterIns)
        def scanOutputs(remaining: BigInt, offset: BigInt, foundPay: Boolean): Boolean =
            if remaining == BigInt(0) then foundPay
            else
                val amount = byteStringToInteger(false, rawTx.slice(offset, 8))
                val lenAndAfter = BitcoinHelpers.readVarInt(rawTx, offset + 8)
                val scriptLen = lenAndAfter._1
                val afterVarInt = lenAndAfter._2
                val script = rawTx.slice(afterVarInt, scriptLen)
                val nextOffset = afterVarInt + scriptLen
                scanOutputs(
                  remaining - 1,
                  nextOffset,
                  foundPay || (script == destinationSpk && amount == pegOutAmount)
                )

        treasurySpent && scanOutputs(outsNumAndOffset._1, outsNumAndOffset._2, false)
    }
}

object PegOutProducedVerifierContract extends Contract {
    given opts: Options = Options.release

    lazy val compiled: PlutusV3[Data => Unit] =
        PlutusV3.compile((scData: Data) => PegOutProducedVerifier.validate(scData))

    /** Blueprint-loaded script (param-free, loaded verbatim). */
    lazy val pinnedScript: Script.PlutusV3 =
        BinocularBlueprint.script("PegOutProducedVerifierContract")

    lazy val blueprint: Blueprint =
        BinocularBlueprint.paramFreeBlueprint(
          title = "PegOutProducedVerifierContract",
          description =
              "legit_treasury_movement_and_peg_out_produced_verifier (config[13]) — withdraw-based " +
                  "verifier that a TM legitimately fulfilled a peg-out.",
          compiled = compiled
        )
}

/** The `legit_treasury_movement_and_peg_out_not_produced_verifier` — config[8]. Only invoked by the
  * `peg_out.ak::Cancel` (refund) branch, which is out of scope this iteration. It needs a valid,
  * distinct script hash in the config datum but is never withdrawn from on the happy path, so it
  * has no reward account and is intentionally unsatisfiable until the refund path is built.
  * `fail`ing is safe: it cleanly disables `Cancel`.
  */
@Compile
object PegOutNotProducedVerifier {
    def validate(scData: Data): Unit =
        fail("peg-out not-produced verifier: Cancel/refund path not implemented yet")
}

object PegOutNotProducedVerifierContract extends Contract {
    given opts: Options = Options.release

    lazy val compiled: PlutusV3[Data => Unit] =
        PlutusV3.compile((scData: Data) => PegOutNotProducedVerifier.validate(scData))

    /** Blueprint-loaded script (param-free, loaded verbatim). */
    lazy val pinnedScript: Script.PlutusV3 =
        BinocularBlueprint.script("PegOutNotProducedVerifierContract")

    lazy val blueprint: Blueprint =
        BinocularBlueprint.paramFreeBlueprint(
          title = "PegOutNotProducedVerifierContract",
          description =
              "legit_treasury_movement_and_peg_out_not_produced_verifier (config[14]) — " +
                  "intentionally unsatisfiable until the refund path is built.",
          compiled = compiled
        )
}
