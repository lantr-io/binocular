package binocular

import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v1.Credential
import scalus.cardano.onchain.plutus.v3.*
import scalus.compiler.{Compile, Options}
import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.PlutusV3

/** Stake validator that verifies a Bitcoin Treasury Movement (TM) transaction actually consumes a
  * specific peg-in deposit via the correct Taproot key-path spend.
  *
  * Used by Bifrost's `peg_in.ak` via the stake validator delegation pattern. The Aiken side passes
  * a redeemer (as a Data list) containing:
  *   - [0] source_chain_treasury_utxo_id: ByteString (36 bytes: prev_txid(32) + prev_vout(4), LE)
  *   - [1] source_chain_raw_peg_in_tx: ByteString (non-witness serialized peg-in tx)
  *   - [2] peg_in_amount: Integer (satoshis)
  *   - [3] treasury_movement_raw_tx: ByteString (witness-serialized TM tx)
  *
  * The Aiken caller verifies these fields match the PegInDatum. This validator's job is Bitcoin
  * transaction parsing and witness inspection:
  *   1. TM tx's first input must spend the treasury UTXO (outpoint match)
  *   2. Treasury input witness must be a Taproot key-path spend (Y_51 FROST signed it)
  *   3. Some TM input must have prev_txid == sha256d(raw_peg_in_tx) (peg-in consumed)
  *   4. Peg-in input witness must be a Taproot key-path spend (same Y_51 FROST, not CSV timeout)
  *
  * Checks 2 and 4 together prove the same FROST federation (Y_51) signed both inputs, ruling out
  * depositor-refund (4320-block CSV) and federation-emergency script-path spends.
  */
@Compile
object PegInVerifierValidator {

    /** Verify that a Treasury Movement tx is legitimate and spends the given peg-in deposit via
      * key-path (not a CSV timeout script path).
      *
      * @param treasuryUtxoId
      *   36-byte outpoint (txid + vout) of the treasury UTXO that the TM must spend as first input
      * @param rawPegInTx
      *   Non-witness serialized Bitcoin peg-in transaction bytes (used to compute txid via sha256d)
      * @param rawTmTx
      *   Witness-serialized Bitcoin Treasury Movement transaction bytes
      */
    def verifyTmSpendsPegIn(
        treasuryUtxoId: ByteString,
        rawPegInTx: ByteString,
        rawTmTx: ByteString
    ): Unit = {
        // 1. TM's first input must spend the treasury UTXO (full 36-byte outpoint match)
        val firstOutpoint = BitcoinHelpers.firstInputOutpoint(rawTmTx)
        require(firstOutpoint == treasuryUtxoId, "TM first input does not match treasury UTXO")

        // 2. Treasury (input 0) must be a Taproot key-path spend — proves Y_51 FROST signed it
        require(BitcoinHelpers.isKeyPathWitness(rawTmTx, 0), "Treasury not spent via key path")

        // 3. TM must consume the peg-in tx; find its index among TM inputs
        val pegInTxHash = sha2_256(sha2_256(rawPegInTx))
        val pegInIdx = BitcoinHelpers.findPegInInputIndex(rawTmTx, pegInTxHash)

        // 4. Peg-in input must also be a key-path spend — rules out depositor CSV refund and
        //    federation emergency path; proves same Y_51 FROST co-signed
        require(BitcoinHelpers.isKeyPathWitness(rawTmTx, pegInIdx), "Peg-in not spent via key path")
    }

    /** Main validator entry point - rewarding (withdraw) validator function.
      *
      * Parses redeemer as a Data list and delegates to [[verifyTmSpendsPegIn]].
      */
    inline def reward(
        redeemer: Datum,
        credential: Credential,
        tx: TxInfo
    ): Unit = {
        val fields = unListData(redeemer)
        val treasuryUtxoId = unBData(fields.head)
        val rawPegInTx = unBData(fields.tail.head)
        // fields.tail.tail.head is peg_in_amount — not needed for this verification
        val rawTmTx = unBData(fields.tail.tail.tail.head)

        verifyTmSpendsPegIn(treasuryUtxoId, rawPegInTx, rawTmTx)
    }

    /** Raw ScriptContext entry point for Scalus compilation. */
    def validate(scData: Data): Unit = {
        val sc = unConstrData(scData).snd
        val txInfoData = sc.head
        val redeemer = sc.tail.head
        val scriptInfo = unConstrData(sc.tail.tail.head)
        // ScriptInfo.RewardingScript has constructor index 2
        if scriptInfo.fst == BigInt(2) then
            val credential = scriptInfo.snd.head.to[Credential]
            val txInfo = txInfoData.to[TxInfo]
            reward(redeemer, credential, txInfo)
        else fail("Not a rewarding script")
    }
}

object PegInVerifierContract {
    given opts: Options = Options.release

    lazy val contract: PlutusV3[Data => Unit] =
        PlutusV3.compile(PegInVerifierValidator.validate)
}
