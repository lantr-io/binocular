package binocular.cli.commands

import binocular.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}
import binocular.server.ProofService

import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, LedgerToPlutusTranslation, TransactionHash, TransactionInput}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.uplc.builtin.ByteString

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** B1: build + submit the peg-in completion tx — mint `peg_in_amount` fBTC to `--recipient` and
  * record the peg-in in the completed-peg-ins MPF. See [[PegInCompleteTx]] for the tx shape and the
  * on-chain requirements (`peg_in.ak::withdraw(CompletePegIn)`).
  *
  * Rev 5.4: there is no `Confirmed` TM record any more ([OB-5]). This references the **bridge state
  * singleton** — the UTxO carrying the NFT `(bridge_state_policy, "BSS")`, where
  * `bridge_state_policy` is Config datum field 3 read at runtime ([CPI-10], [PAR-1]) — and proves
  * the sweep with the [CPI-9] MPF membership proof of `(peg_in_utxo_id -> sweeping_tm_input_0)`
  * against the singleton's `spi_root`. The proof and the proven value come from
  * [[SweptPegInsProofService]] ([OB-10]).
  *
  * Permissionless except for the depositor's BIP-322 signature, which is produced externally (e.g.
  * with `heimdall/.keys/alice.wif` via `sign-pegin-msg`) and passed via `--signature`. The command
  * prints the exact 32-byte message digest to sign: `sha2_256("BFR-mint-v1" ‖ peg_in_utxo_id ‖
  * serialiseData(recipient))` ([CPI-3], [OB-11]). The retired preimage also carried the TM txid;
  * the message no longer names a transaction, so the depositor MAY sign before the sweep.
  *
  * Preconditions (one-time setup): the peg_in withdraw reward cred is registered
  * (`register-bridge-creds`), the sweeping TM has been confirmed (so the singleton's `spi_root`
  * covers this deposit). The completed set is reconstructed from chain history and checked against
  * the live root; `--prior-pegin` remains an optional manual override supplying the full set.
  */
case class PegInCompleteCommand(
    pirRef: String,
    recipient: String,
    signature: Option[String],
    priorPegins: List[String] = Nil,
    dryRun: Boolean = false
) extends Command {

    override def execute(config: BinocularConfig): Int = boundary {
        Console.header("Binocular Peg-In Complete (mint fBTC)")
        if dryRun then Console.warn("Dry-run mode — will assemble but not submit")
        println()

        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        def hexBytes(label: String, s: String, expectedChars: Option[Int]): ByteString = {
            val isHex = s.length % 2 == 0 && s.forall(c => "0123456789abcdefABCDEF".contains(c))
            if !isHex || expectedChars.exists(_ != s.length) then {
                val want = expectedChars.fold("even-length hex")(n => s"$n hex chars")
                Console.error(s"Invalid $label: expected $want, got '$s'"); break(1)
            }
            ByteString.fromHex(s)
        }
        def parseRef(label: String, s: String): TransactionInput = s.split("#") match {
            case Array(h, i) if i.toIntOption.isDefined =>
                TransactionInput(TransactionHash.fromHex(h), i.toInt)
            case _ => Console.error(s"Invalid $label: expected TX_HASH#INDEX, got '$s'"); break(1)
        }

        // Validate the signature's format up front if supplied, but don't *require* it yet: the
        // intended flow is `--dry-run` (no signature) to print the digest, sign it, then re-run with
        // --signature. Presence is enforced only for the real (non-dry-run) submit, below.
        val sigBytesOpt: Option[ByteString] = signature.map(hexBytes("signature", _, Some(128)))
        val pirInput = parseRef("--pir", pirRef)
        // Resolve the recipient all the way to its plutus form here, inside the guard, so a
        // bech32-valid but non-payment address (stake/Byron) fails cleanly rather than throwing an
        // uncaught exception later when getAddress runs.
        val recipientLedger =
            try {
                val addr = Address.fromBech32(recipient)
                LedgerToPlutusTranslation.getAddress(addr)
                addr
            } catch {
                case e: Exception =>
                    Console.error(
                      s"Invalid --recipient (must be a bech32 payment address): ${e.getMessage}"
                    )
                    break(1)
            }

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err); break(1)
        }
        val provider = setup.provider
        val network = setup.network
        val oracleScriptHash = setup.script.scriptHash

        val (blueprint, blueprintSource) =
            try BifrostBlueprint.resolve(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(s"Loading bridge blueprint: ${e.getMessage}"); break(1)
            }
        Console.info("blueprint", blueprintSource)

        // --- bridge config / scripts ---
        val configNftPolicy =
            hexBytes("bridge.config-nft-policy-id", config.bridge.configNftPolicyId, Some(56))
        hexBytes("bridge.config-nft-asset-name", config.bridge.configNftAssetName, None)
        val bridgedTokenAsset =
            AssetName(
              hexBytes("bridge.bridged-token-asset-name", config.bridge.bridgedTokenAssetName, None)
            )
        if config.bridge.completedPegInsOneShotRef.isEmpty then {
            Console.error(
              "Set binocular.bridge.completed-peg-ins-one-shot-ref (the cpi one-shot from deploy-bridge)"
            )
            break(1)
        }
        val cpiRefInput = parseRef(
          "bridge.completed-peg-ins-one-shot-ref",
          config.bridge.completedPegInsOneShotRef
        )
        val cpiRef = TxOutRef(TxId(cpiRefInput.transactionId), cpiRefInput.index)

        val oraclePolicyBS = ByteString.fromArray(oracleScriptHash.bytes)
        val pegIn =
            PegInContract(blueprint, oraclePolicyBS, configNftPolicy)
        val cpiContract =
            CompletedPegInsContract(blueprint, configNftPolicy, cpiRef)
        val cpiPolicy = cpiContract.policyId
        val bridgedToken = BridgedTokenContract(blueprint, configNftPolicy)

        Console.info("Peg-in policy", pegIn.policyId.toHex)
        Console.info("fBTC policy", bridgedToken.policyId.toHex)
        Console.info("completed-peg-ins policy", cpiPolicy.toHex)
        println()

        val completion = PegInCompletion(pegIn, cpiContract, bridgedToken, bridgedTokenAsset)
        val history = ProviderChainHistory.from(provider, timeout).valueOr { err =>
            Console.error(err); break(1)
        }
        Console.step(1, "Preparing sweep and completed-peg-ins proofs")
        val snapshot = ProofService
            .fromConfig(config, msg => Console.info("spi", msg))
            .valueOr { err =>
                Console.error(err); break(1)
            }
            .sweptSnapshot()
            .valueOr { err =>
                Console.error(err.message); break(1)
            }
        val prepared = completion
            .prepare(
              provider,
              history,
              snapshot,
              pirInput,
              recipientLedger,
              timeout,
              priorPegins.map(k => hexBytes("--prior-pegin", k, None))
            )
            .valueOr { err =>
                Console.error(err); break(1)
            }
        Console.info("sweeping TM input 0", prepared.sweep.sweepingTmInput0.toHex)
        Console.info("Depositor signs (BIP-322 text)", prepared.signText)
        Console.info("  → in a wallet: signMessage(text, \"bip322-simple\")", "")
        Console.info("  digest (for sign-pegin-msg --message)", prepared.digest.toHex)
        println()

        if dryRun then {
            Console.success("Dry-run complete (assembled proofs + redeemers; not building tx)")
            break(0)
        }

        val sigBytes = sigBytesOpt.getOrElse {
            Console.error(
              "--signature is required for a real run. Re-run with --dry-run to print the digest, " +
                  "sign it with `sign-pegin-msg`, then pass --signature <64-byte hex>."
            )
            break(1)
        }

        Console.step(3, "Building + submitting completion tx")
        val tx =
            try {
                val refs = PegInCompletion.references(
                  provider,
                  CommandHelpers.refScriptPairs(
                    config,
                    CommandHelpers.refScriptScanAddresses(config, network, setup.sponsorAddress)
                  ),
                  timeout
                )
                completion.build(provider, setup.hdAccount, prepared, refs, sigBytes).await(timeout)
            } catch {
                case e: Exception =>
                    Console.error(s"Building tx: ${e.getMessage}")
                    Option(e.getCause).foreach(c => Console.error(s"Cause: ${c.getMessage}"))
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
        Console.tx("Peg-in complete TX", txHash)
        Console.info("fBTC minted (sat)", prepared.datum.pegInAmount.toString)
        Console.info("recipient", recipient)
        Console.info("new completed-peg-ins root", prepared.update.newRoot.toHex)
        Console.info("completed peg_in_utxo_id", prepared.datum.pegInUtxoId.toHex)
        Console.separator()
        0
    }
}
