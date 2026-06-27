package binocular.cli.commands

import binocular.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}

import scalus.cardano.address.{StakeAddress, StakePayload}
import scalus.cardano.ledger.{ScriptHash, TransactionHash}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.txbuilder.TxBuilder
import scalus.uplc.builtin.ByteString

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** Register the reward (stake) credentials of the bridge withdraw scripts the completion txs use,
  * so Conway will accept their 0-ADA withdrawals.
  *
  * Peg-in: `withdraw(CompletePegIn)` runs only ONE rewarding script — `peg_in` itself (the
  * stake-validator delegation pattern: the PIR + completed-peg-ins spends and the fBTC mint all
  * require a withdrawal from the peg_in script).
  *
  * Peg-out: `withdraw(CompletePegOut)` runs TWO rewarding scripts — the `peg_out` validator itself
  * (the completed-peg-outs spend + fBTC burn delegate to it) and the real
  * `peg_out_produced_verifier` (config[13]), which `peg_out.ak` invokes via `validate_withdraw`.
  * Both reward accounts are registered here. The not-produced verifier (config[14]) is only
  * withdrawn from on the Cancel path (out of scope), so it is intentionally left unregistered.
  *
  * Conway rejects a withdrawal whose reward account is not registered, and certificates validate
  * against the *pre-transaction* ledger state, so registration must happen in an earlier tx — it
  * cannot be folded into the completion tx.
  *
  * Registration uses the deposit-less Shelley `RegCert` (`registerStake(stakeAddress)`), which does
  * NOT execute the stake script — important because the peg_in script `fail`s on any non-Rewarding
  * purpose. (Same approach as ft-bifrost-bridge's offchain spo-demo
  * `registerBanWithdrawCredential`.)
  *
  * Run after the bridge config + the (re-minted) peg_in policy are fixed. Each credential is
  * registered in its OWN tx and an already-registered one is skipped (not fatal): the
  * config-derived peg_in/peg_out hashes are fresh per deploy, but the produced verifier is a
  * parameterless script whose hash is constant across deploys, so on a redeploy it is already
  * registered while the others are not — per-cred txs let the fresh ones through regardless. The
  * command is therefore safe to re-run. It does NOT touch the config / completed-peg-ins /
  * completed-peg-outs / fBTC NFTs.
  */
case class RegisterBridgeCredsCommand(dryRun: Boolean = false) extends Command {

    override def execute(config: BinocularConfig): Int = boundary {
        Console.header("Register Bridge Withdraw Credentials")
        if dryRun then Console.warn("Dry-run mode — will compute hashes but not submit")
        println()

        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        def hexBytes(label: String, s: String, expectedChars: Option[Int]): ByteString = {
            val isHex = s.length % 2 == 0 && s.forall(c => "0123456789abcdefABCDEF".contains(c))
            if !isHex || expectedChars.exists(_ != s.length) then {
                val want = expectedChars.fold("even-length hex")(n => s"$n hex chars")
                Console.error(s"Invalid $label: expected $want, got '$s'")
                break(1)
            }
            ByteString.fromHex(s)
        }

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err); break(1)
        }
        val provider = setup.provider
        val network = setup.network
        val sponsorAddress = setup.sponsorAddress
        val oraclePolicyId = ByteString.fromArray(setup.script.scriptHash.bytes)

        val blueprint =
            try BifrostBlueprint.fromFile(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(
                      s"Loading bridge blueprint (${config.bridge.plutusJson}): ${e.getMessage}"
                    )
                    break(1)
            }

        val configNftPolicy =
            hexBytes("bridge.config-nft-policy-id", config.bridge.configNftPolicyId, Some(56))
        val configNftAsset =
            hexBytes("bridge.config-nft-asset-name", config.bridge.configNftAssetName, None)

        // peg_in withdraw script (= peg_in policy) — config[10]. Its hash now depends on the TM-NFT
        // policy (peg_in.ak's 4th param), so apply the same value here.
        val pegIn = PegInContract(
          blueprint,
          oraclePolicyId,
          configNftPolicy,
          configNftAsset,
          CommandHelpers.tmNftPolicy(config, setup.script.scriptHash)
        )
        val pegInHash = pegIn.policyId

        // Peg-out completion (`peg_out.ak::CompletePegOut`) withdraws from TWO scripts that need
        // registered reward accounts: the peg_out withdraw validator itself (config[11]) and the
        // real produced verifier (config[13]). The not-produced verifier (config[14]) is only
        // withdrawn from on the Cancel path (out of scope), so it is NOT registered here.
        val pegOut = PegOutContract(blueprint, oraclePolicyId, configNftPolicy, configNftAsset)
        val pegOutHash = pegOut.policyId
        val pegOutProducedVerifierHash =
            PegOutProducedVerifierContract.pinnedScript.scriptHash

        val creds: List[(String, ScriptHash)] = List(
          "peg_in" -> pegInHash,
          "peg_out" -> pegOutHash,
          "peg_out_produced_verifier" -> pegOutProducedVerifierHash
        )

        Console.info("Oracle policy", oraclePolicyId.toHex)
        creds.foreach { case (name, h) =>
            val stakeAddr = StakeAddress(network, StakePayload.Script(h))
            Console.info(s"$name reward acct", stakeAddr.encode.getOrElse(h.toHex))
        }
        println()

        if dryRun then {
            Console.success("Dry-run complete (computed reward-account hashes; not submitting)")
            break(0)
        }

        // Register each credential in its OWN tx, tolerating an already-registered one. The peg_in
        // and peg_out hashes are config-derived (fresh per deploy), but the produced verifier is a
        // parameterless script whose hash is CONSTANT across every deploy — so on any redeploy it is
        // already registered. A single atomic multi-RegCert tx would then be rejected wholesale,
        // leaving the fresh peg_in/peg_out creds unregistered. Per-cred txs let each one that is not
        // yet registered succeed independently. Registering an already-registered credential fails
        // at build or submit; we treat that as "already done" and continue.
        val signer = setup.hdAccount.signerForUtxos
        val registered = scala.collection.mutable.ListBuffer.empty[String]
        val skipped = scala.collection.mutable.ListBuffer.empty[String]

        creds.zipWithIndex.foreach { case ((name, h), i) =>
            Console.step(i + 1, s"Registering reward credential: $name")
            val txOpt =
                try
                    Some(
                      TxBuilder(provider.cardanoInfo)
                          .registerStake(StakeAddress(network, StakePayload.Script(h)))
                          .complete(provider, sponsorAddress)
                          .await(timeout)
                          .sign(signer)
                          .transaction
                    )
                catch {
                    case e: Exception =>
                        Console.warn(
                          s"$name: build failed (likely already registered) — skipping: ${e.getMessage}"
                        )
                        skipped += name
                        None
                }

            txOpt.foreach { tx =>
                OracleTransactions.submitTx(provider, tx, timeout) match {
                    case Left(err) =>
                        Console.warn(
                          s"$name: submit failed (likely already registered) — skipping: $err"
                        )
                        skipped += name
                    case Right(txHash) =>
                        // await window MUST exceed the poll budget (attempts*delayMs); otherwise it
                        // preempts the poll and throws even when the tx confirms (seen on preprod —
                        // the verifier reg tx confirmed just after a 120s await gave up). See
                        // DeployBridgeCommand.confirmAwait.
                        val status = provider
                            .pollForConfirmation(
                              TransactionHash.fromHex(txHash),
                              maxAttempts = DeployBridgeCommand.ConfirmPollAttempts,
                              delayMs = DeployBridgeCommand.ConfirmPollDelayMs
                            )
                            .await(DeployBridgeCommand.confirmAwait)
                        status match {
                            case TransactionStatus.Confirmed =>
                                Console.tx(s"$name registration TX", txHash)
                                registered += name
                            case other =>
                                Console.error(s"$name: not confirmed: $other"); break(1)
                        }
                }
            }
        }

        println()
        Console.separator()
        if registered.nonEmpty then Console.success(s"Registered: ${registered.mkString(", ")}.")
        if skipped.nonEmpty then
            Console.warn(s"Skipped (already registered or unbuildable): ${skipped.mkString(", ")}.")
        if registered.isEmpty && skipped.nonEmpty then
            Console.warn("Nothing newly registered — all creds were already registered.")
        Console.separator()
        0
    }
}
