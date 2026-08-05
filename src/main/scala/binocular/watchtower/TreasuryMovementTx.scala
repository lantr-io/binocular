package binocular.watchtower

import binocular.cli.Console
import binocular.oracle.OracleTransactions
import scalus.cardano.address.Address
import scalus.cardano.ledger.{TransactionHash, Utxo}
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
  * Spends an `Unconfirmed` TM UTxO with the real treasury-movement validator, references the
  * Binocular oracle UTxO (so the validator can check the block is confirmed), and recreates the
  * UTxO at the same address with the `Confirmed` datum — preserving the value so the TM marker
  * token rides along (the validator enforces this).
  *
  * The Confirm branch also requires a Config reference input (it reads the completed-peg-outs trie
  * policy from field 3) and requires the completed-peg-outs trie UTxO to be SPENT and recreated at
  * the same address carrying the root the TM's `"CPOR1"` output attests. Both are supplied here
  * from a [[TrieSpend]] the caller assembles (see `ConfirmTmtxCommand`), so the tx satisfies the
  * trie's own Aiken validator too: it gates the spend on exactly this `Unconfirmed -> Confirmed`
  * transition.
  */
object TreasuryMovementTx {

    /** The completed-peg-outs trie parts of a Confirm tx.
      *
      * @param utxo
      *   the trie UTxO to spend, located by the `(field-3 policy, "CPO")` NFT.
      * @param script
      *   `completed_peg_outs_merkle_tree_validator` applied to `(TM script hash, one-shot ref)`.
      *   Its hash MUST equal Config field 3, or the ledger rejects the spend.
      * @param newDatum
      *   the recreated trie datum carrying the root this TM's `"CPOR1"` commitment output attests.
      */
    final case class TrieSpend(
        utxo: Utxo,
        script: scalus.cardano.ledger.Script.PlutusV3,
        newDatum: Data
    )

    /** Build, sign, submit, and await the Confirm tx. Returns the Cardano tx hash on success. */
    def buildAndSubmitConfirm(
        provider: BlockchainProvider,
        hdAccount: HdAccount,
        tmScript: scalus.cardano.ledger.Script.PlutusV3,
        tmAddress: Address,
        unconfirmed: Utxo,
        oracle: Utxo,
        configUtxo: Utxo,
        trie: TrieSpend,
        redeemer: Data,
        confirmedDatum: Data,
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
            // The trie redeemer is `Data.unit`: the Aiken trie validator's spend handler ignores
            // both datum and redeemer and reads only the TM NFT tag transition from the tx.
            //
            // The trie output preserves the input's whole value, so the "CPO" NFT and its min-ADA
            // ride along and the TM validator's `trieOut.address === trieIn.address` check holds.
            val tx = TxBuilder(provider.cardanoInfo)
                .spend(unconfirmed, redeemer, tmScript)
                .spend(trie.utxo, Data.unit, trie.script)
                .references(oracle)
                .references(configUtxo)
                .payTo(tmAddress, unconfirmed.output.value, confirmedDatum)
                .payTo(trie.utxo.output.address, trie.utxo.output.value, trie.newDatum)
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
