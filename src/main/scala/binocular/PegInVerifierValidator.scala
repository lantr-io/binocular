package binocular

import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v1.Credential
import scalus.cardano.onchain.plutus.v3.*
import scalus.compiler.{Compile, Options}
import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.PlutusV3

/** Stake validator that verifies a Bitcoin Treasury Movement (TM) transaction actually consumes a
  * specific peg-in deposit via a valid Bifrost protocol spending path.
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
  *   2. Some TM input must have prev_txid == sha256d(raw_peg_in_tx) (peg-in consumed)
  *   3. The spending mode must be one of the three valid Bifrost protocol modes:
  *        - 51% (main line):    treasury key-path Y_51,   peg-in key-path Y_51
  *        - 67% (aspirational): treasury script-path Y_67, peg-in key-path Y_51
  *        - Federation:         treasury script-path Y_fed, peg-in script-path Y_fed
  *
  * The mode check is: if treasury is key-path then peg-in must also be key-path.
  * This rules out the only invalid combination (key-path treasury + script-path peg-in),
  * which would indicate a depositor CSV refund or other non-protocol spend.
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

        // 2. Find the peg-in input index before the witness check
        val pegInTxHash = sha2_256(sha2_256(rawPegInTx))
        val pegInIdx = BitcoinHelpers.findPegInInputIndex(rawTmTx, pegInTxHash)

        // 3. Verify the spending mode matches one of the three valid protocol modes:
        //      51% (main line)    — treasury key-path  Y_51,  peg-in key-path  Y_51
        //      67% (aspirational) — treasury script-path Y_67, peg-in key-path  Y_51
        //      Federation         — treasury script-path Y_fed, peg-in script-path Y_fed
        //    The one forbidden combination is treasury key-path with peg-in script-path:
        //    that would indicate a depositor CSV refund or other non-federation script path.
        val treasuryIsKeyPath = BitcoinHelpers.isKeyPathWitness(rawTmTx, 0)
        val pegInIsKeyPath = BitcoinHelpers.isKeyPathWitness(rawTmTx, pegInIdx)
        require(!treasuryIsKeyPath || pegInIsKeyPath, "Key-path treasury requires key-path peg-in")
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

    inline def validate(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.RewardingScript(credential) =>
                reward(sc.redeemer, credential, sc.txInfo)
            case _ => fail("Not a rewarding script")
    }
}

object PegInVerifierContract {
    given opts: Options = Options.release

    lazy val contract: PlutusV3[Data => Unit] =
        PlutusV3.compile(PegInVerifierValidator.validate)
}
