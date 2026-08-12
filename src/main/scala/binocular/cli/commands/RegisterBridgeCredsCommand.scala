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
  * Exactly TWO reward accounts exist under spec rev 5.1, and both are registered here:
  *   - `peg_in` (config[4]) – `withdraw(CompletePegIn)` runs it as the single rewarding script (the
  *     stake-validator delegation pattern: the PIR + completed-peg-ins spends require a withdrawal
  *     from the peg_in script). The `bridged_token` policy reads the ConfigDatum and enforces the
  *     mint against this same withdrawal directly (Variant B – no separate mint checker).
  *   - `peg_out` (config[5]) – `withdraw(CompletePegOut)` runs it as the single rewarding script,
  *     and the PegOutRequest spend delegates to it.
  *
  * Nothing else is registered. The produced verifier (config[7]) and the not-produced verifier
  * (config[8]) are RETIRED under the attested-root scheme: the rewritten `peg_out.ak` proves
  * payment against the quorum-attested completed-peg-outs trie root and never delegates to either,
  * so neither is ever withdrawn from and neither needs a reward account. `deploy-bridge` still
  * writes their hashes into the config datum only because the datum shape kept those slots.
  *
  * Conway rejects a withdrawal whose reward account is not registered, and certificates validate
  * against the *pre-transaction* ledger state, so registration must happen in an earlier tx – it
  * cannot be folded into the completion tx.
  *
  * Registration uses the deposit-less Shelley `RegCert` (`registerStake(stakeAddress)`), which does
  * NOT execute the stake script – important because the peg_in script `fail`s on any non-Rewarding
  * purpose. (Same approach as ft-bifrost-bridge's offchain spo-demo
  * `registerBanWithdrawCredential`.)
  *
  * Run after the bridge config + the (re-minted) peg_in policy are fixed. `deploy-bridge` already
  * registers both accounts inside its bootstrap tx, so this command is the idempotent repair path:
  * each credential is registered in its OWN tx and an already-registered one is skipped (not
  * fatal), so a partially-registered deployment converges instead of failing wholesale. The command
  * is therefore safe to re-run. It does NOT touch the config / completed-peg-ins /
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

        val (blueprint, blueprintSource) =
            try BifrostBlueprint.resolve(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(s"Loading bridge blueprint: ${e.getMessage}"); break(1)
            }
        Console.info("blueprint", blueprintSource)

        val configNftPolicy =
            hexBytes("bridge.config-nft-policy-id", config.bridge.configNftPolicyId, Some(56))
        val configNftAsset =
            hexBytes("bridge.config-nft-asset-name", config.bridge.configNftAssetName, None)

        // peg_in withdraw script (= peg_in policy) – config[6] since the rev-5.5 re-index. Rev
        // 5.4 dropped its tm_nft_policy_id param and rev 5.5 dropped the config NFT asset name
        // ([CFG-7]), so the hash depends only on the oracle and the config NFT policy.
        val pegIn = PegInContract(blueprint, oraclePolicyId, configNftPolicy)
        val pegInHash = pegIn.policyId

        // Peg-out completion (`peg_out.ak::CompletePegOut`) withdraws from exactly ONE script: the
        // peg_out withdraw validator itself (config[7] after the rev-5.5 re-index). The rev-5.1
        // verifier slots are gone, so nothing else is ever withdrawn from or registered.
        val pegOut = PegOutContract(blueprint, configNftPolicy)
        val pegOutHash = pegOut.policyId

        val creds: List[(String, ScriptHash)] = List(
          "peg_in" -> pegInHash,
          "peg_out" -> pegOutHash
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

        // Register each credential in its OWN tx, tolerating an already-registered one. Both hashes
        // are config-derived (fresh per deploy) and `deploy-bridge` normally registers them in its
        // bootstrap tx, so the usual outcome here is "both already registered". A single atomic
        // multi-RegCert tx would then be rejected wholesale, leaving a genuinely missing credential
        // unregistered. Per-cred txs let each one that is not yet registered succeed independently.
        // Registering an already-registered credential fails at build or submit; we treat that as
        // "already done" and continue.
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
                        // preempts the poll and throws even when the tx confirms (seen on preprod –
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
