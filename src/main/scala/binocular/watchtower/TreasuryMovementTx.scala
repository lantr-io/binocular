package binocular.watchtower

import binocular.cli.Console
import binocular.oracle.OracleTransactions
import scalus.cardano.ledger.{AssetName, TransactionHash, Utxo}
import scalus.cardano.node.{BlockchainProvider, TransactionStatus}
import scalus.cardano.txbuilder.TxBuilder
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.DebugScript
import scalus.uplc.builtin.Data
import scalus.utils.await

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.util.chaining.scalaUtilChainingOps

/** Off-chain builder for the validated TM Confirm transaction (the on-chain counterpart is
  * [[TreasuryMovementValidator]]).
  *
  * Rev 5.4 shape (spec §Confirm TM tx): spend the `Unconfirmed` TM UTxO with
  * `TmSpendRedeemer.Confirm(proof)`, spend the bridge-state singleton, reference the Binocular
  * oracle and the Config UTxO, burn the TM NFT ([CTM-24]), produce NO output at the TM address
  * ([CTM-25]), and recreate the singleton at its own address carrying the [[BridgeState]] this TM
  * attests ([CTM-27]). The singleton's own validator ([BSS-1]/[BSS-2]) admits the spend because the
  * TM input's redeemer is `Confirm`.
  */
object TreasuryMovementTx {

    /** The bridge-state singleton parts of a Confirm tx.
      *
      * @param utxo
      *   the singleton UTxO to spend, located by the `(Config bridge_state_policy, "BSS")` NFT.
      * @param script
      *   `bridge_state` applied to `(TM script hash, one-shot ref)`. Its hash MUST equal Config
      *   field 3 (`bridge_state_policy`), or the ledger rejects the spend.
      * @param newDatum
      *   the recreated singleton datum: the [[BridgeState]] the TM's `"BTMR1"` commitment output
      *   attests, with the head and amount of its new treasury output.
      */
    final case class SingletonSpend(
        utxo: Utxo,
        script: scalus.cardano.ledger.Script.PlutusV3,
        newDatum: Data
    )

    /** Build, sign, submit, and await the Confirm tx. Returns the Cardano tx hash on success. */
    def buildAndSubmitConfirm(
        provider: BlockchainProvider,
        hdAccount: HdAccount,
        tmScript: scalus.cardano.ledger.Script.PlutusV3,
        unconfirmed: Utxo,
        oracle: Utxo,
        configUtxo: Utxo,
        singleton: SingletonSpend,
        confirmRedeemer: Data,
        timeout: Duration,
        debugTmScript: Option[scalus.cardano.ledger.Script.PlutusV3] = None
    )(using ExecutionContext): Either[String, String] = {
        val signer = hdAccount.signerForUtxos
        val sponsorAddress = hdAccount.baseAddress(provider.cardanoInfo.network)
        try {
            Console.log("  Building TM Confirm transaction...")
            // Diagnostic: when a trace-compiled twin is supplied (TM_DEBUG_TRACE), register it under
            // the deployed TM hash so Scalus replays a failing eval WITH trace strings.
            //
            // The singleton's redeemer is `Data.unit`: bridge-state.ak's spend handler ignores its
            // own datum and redeemer — it discriminates on the TM input's redeemer ([BSS-2]).
            //
            // The TM NFT burn's redeemer is `Data.unit` too: the mint handler's burn branch reads
            // no redeemer (every check lives in `spend`).
            //
            // The singleton output preserves the input's whole value, so the "BSS" NFT and its
            // min-ADA ride along and [CTM-29] holds. There is deliberately NO payTo the TM address.
            val tx = TxBuilder(provider.cardanoInfo)
                .spend(unconfirmed, confirmRedeemer, tmScript)
                .spend(singleton.utxo, Data.unit, singleton.script)
                .references(oracle)
                .references(configUtxo)
                .mint(tmScript, Map(AssetName.empty -> -1L), Data.unit)
                .payTo(
                  singleton.utxo.output.address,
                  singleton.utxo.output.value,
                  singleton.newDatum
                )
                .pipe(b =>
                    debugTmScript.fold(b)(ds =>
                        b.withDebugScript(tmScript.scriptHash, DebugScript(ds))
                    )
                )
                .complete(provider, sponsorAddress)
                .await(timeout)
                .sign(signer)
                .transaction

            Console.log("  Submitting...")
            val txHash = OracleTransactions.submitTx(provider, tx, timeout) match {
                case Right(hash) => hash
                case Left(err)   => return Left(err)
            }

            Console.log(s"  Submitted: $txHash — waiting for Cardano confirmation...")
            provider
                .pollForConfirmation(
                  TransactionHash.fromHex(txHash),
                  maxAttempts = 60,
                  delayMs = 2000
                )
                .await(timeout) match {
                case TransactionStatus.Confirmed => Right(txHash)
                case other                       => Left(s"Transaction status: $other")
            }
        } catch {
            case e: Exception => Left(e.getMessage)
        }
    }
}
