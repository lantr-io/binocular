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

/** Register the reward (stake) credentials of the three withdraw scripts the peg-in completion tx
  * uses, so Conway will accept the 0-ADA withdrawals from them.
  *
  * The `withdraw(CompletePegIn)` path runs three rewarding scripts — `peg_in`, `owner_auth`
  * ([[PegInDepositorAuthValidator]]) and `legit_TM_verifier` ([[PegInVerifierValidator]]). Conway
  * rejects a withdrawal whose reward account is not registered, and certificates are validated
  * against the *pre-transaction* ledger state, so registration must happen in an earlier tx — it
  * cannot be folded into the completion tx itself.
  *
  * Registration uses the deposit-less Shelley `RegCert` (`registerStake(stakeAddress)`), which does
  * NOT execute the stake script — important here, because all three scripts `fail` on any
  * non-Rewarding purpose. (Same approach as ft-bifrost-bridge's offchain spo-demo
  * `registerBanWithdrawCredential`.)
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
        val bridgedTokenPolicy =
            hexBytes("bridge.bridged-token-policy-id", config.bridge.bridgedTokenPolicyId, Some(56))
        val bridgedTokenAsset =
            hexBytes("bridge.bridged-token-asset-name", config.bridge.bridgedTokenAssetName, None)

        // peg_in withdraw script (= peg_in policy) — config[10].
        val pegIn = PegInContract(blueprint, oraclePolicyId, configNftPolicy, configNftAsset)
        val pegInHash = pegIn.policyId

        // owner_auth — recipient-binding depositor-auth withdraw, parameterized by peg_in hash +
        // the fBTC asset.
        val ownerAuthHash = PegInDepositorAuthContract
            .makeContract(
              PegInDepositorAuthParams(
                pegInScriptHash = ByteString.fromArray(pegInHash.bytes),
                bridgedTokenPolicyId = bridgedTokenPolicy,
                bridgedTokenAssetName = bridgedTokenAsset
              )
            )
            .script
            .scriptHash

        // legit_TM_verifier — fixed (unparameterized) — config[12].
        val legitTmHash = PegInVerifierContract.contract.script.scriptHash

        val creds: List[(String, ScriptHash)] = List(
          "peg_in" -> pegInHash,
          "owner_auth" -> ownerAuthHash,
          "legit_TM_verifier" -> legitTmHash
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

        Console.step(1, "Registering 3 withdraw reward credentials")
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
            case other => Console.error(s"Not confirmed: $other"); break(1)
        }

        println()
        Console.separator()
        Console.tx("Registration TX", txHash)
        Console.success("All 3 withdraw reward credentials registered.")
        Console.separator()
        0
    }
}
