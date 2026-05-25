package binocular.cli.commands

import binocular.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}

import scalus.cardano.address.{StakeAddress, StakePayload}
import scalus.cardano.ledger.{ScriptHash, Transaction, TransactionHash}
import scalus.cardano.node.{BlockchainProvider, TransactionStatus}
import scalus.cardano.txbuilder.TxBuilder
import scalus.uplc.builtin.ByteString

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** Register the reward (stake) credential of the `peg_in` withdraw script the peg-in completion tx
  * uses, so Conway will accept its 0-ADA withdrawal.
  *
  * Since B1 (Confirmed-TM reference) the `withdraw(CompletePegIn)` path runs only ONE rewarding
  * script — `peg_in` itself (the stake-validator delegation pattern: the PIR + completed-peg-ins
  * spends and the fBTC mint all require a withdrawal from the peg_in script). The former
  * `owner_auth` and `legit_TM_verifier` withdrawals are gone (depositor auth is embedded in
  * `peg_in.ak`, and the TM proof moved to `confirm-tmtx`). Conway rejects a withdrawal whose reward
  * account is not registered, and certificates validate against the *pre-transaction* ledger state,
  * so registration must happen in an earlier tx — it cannot be folded into the completion tx.
  *
  * Registration uses the deposit-less Shelley `RegCert` (`registerStake(stakeAddress)`), which does
  * NOT execute the stake script — important because the peg_in script `fail`s on any non-Rewarding
  * purpose. (Same approach as ft-bifrost-bridge's offchain spo-demo `registerBanWithdrawCredential`.)
  *
  * This is a one-shot setup step: registering an already-registered credential makes the tx fail,
  * so run it once after the bridge config + the (re-minted) peg_in policy are fixed. It does NOT
  * touch the config / completed-peg-ins / fBTC NFTs.
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

        val creds: List[(String, ScriptHash)] = List("peg_in" -> pegInHash)

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

        Console.step(1, "Registering the peg_in withdraw reward credential")
        val signer = setup.hdAccount.signerForUtxos
        val tx =
            try {
                val builder = creds.foldLeft(TxBuilder(provider.cardanoInfo)) { (b, c) =>
                    b.registerStake(StakeAddress(network, StakePayload.Script(c._2)))
                }
                builder
                    .complete(provider, sponsorAddress)
                    .await(timeout)
                    .sign(signer)
                    .transaction
            } catch {
                case e: Exception =>
                    Console.error(s"Building registration tx: ${e.getMessage}")
                    Console.error(
                      "If a credential is already registered the tx fails — this step is one-shot."
                    )
                    break(1)
            }

        val txHash = OracleTransactions.submitTx(provider, tx, timeout) match {
            case Right(h)  => h
            case Left(err) => Console.error(s"Submit: $err"); break(1)
        }
        val status = provider
            .pollForConfirmation(TransactionHash.fromHex(txHash), maxAttempts = 60, delayMs = 2000)
            .await(timeout)
        status match {
            case TransactionStatus.Confirmed =>
            case other                       => Console.error(s"Not confirmed: $other"); break(1)
        }

        println()
        Console.separator()
        Console.tx("Registration TX", txHash)
        Console.success("peg_in withdraw reward credential registered.")
        Console.separator()
        0
    }
}
