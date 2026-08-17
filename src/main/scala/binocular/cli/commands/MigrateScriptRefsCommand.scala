package binocular.cli.commands

import binocular.*
import binocular.cli.{Command, CommandHelpers, Console}

import io.bullet.borer.Cbor
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Script, ScriptHash, ScriptRef, Transaction, TransactionHash, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.node.TransactionStatus
import scalus.cardano.txbuilder.{TransactionBuilderStep, TxBuilder}
import scalus.uplc.builtin.ByteString

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.Try
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** One-off relocation of the CIP-33 reference-script UTxOs that earlier `deploy-script-refs` runs
  * left at the sponsor WALLET address, over to the native `sig(walletKey)` holding address
  * ([[binocular.cli.CommandHelpers.refScriptHoldingAddress]]).
  *
  * Why they must move: a ref UTxO sitting at the wallet address is a coin-selection candidate.
  * Every transaction the daemon builds can pick a 50 ADA ref UTxO as a fee input — which
  * under-prices the Conway per-byte reference-script surcharge (FeeTooSmallUTxO) and, if it ever
  * succeeds, destroys a deployed reference script. At a script address it is simply not selectable,
  * while the wallet key alone still satisfies the native script, so the ADA stays reclaimable.
  *
  * One transaction per UTxO, run serially: the largest bridge script is ~13.7 KB, so a single
  * script per tx stays well under Cardano's 16 KB limit while two would not. Each tx spends the ref
  * UTxO (enriched with its `scriptRef`, so the surcharge is priced — see [[fetchScript]]) and
  * recreates an identical output, same Value and same ScriptRef, at the holding address. Fees and
  * change come from clean sponsor UTxOs: every ref outpoint found at EITHER scanned address is
  * excluded from selection.
  *
  * Idempotent by construction: what it moves is what the source scan still finds at the wallet, so
  * a re-run after a partial failure moves only the remainder, and a run with nothing left to move
  * exits 0.
  */
case class MigrateScriptRefsCommand(dryRun: Boolean = false) extends Command {

    import MigrateScriptRefsCommand.*

    override def execute(config: BinocularConfig): Int = boundary {
        Console.header("Migrate Reference Scripts to the Holding Address")
        if dryRun then Console.warn("Dry-run mode — will resolve every move but not submit")
        println()

        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err); break(1)
        }
        val provider = setup.provider
        val network = setup.network
        val sponsorAddress = setup.sponsorAddress
        val signer = setup.signer
        val sponsorBech32 = sponsorAddress.encode.getOrElse {
            Console.error(s"Cannot encode the sponsor address: $sponsorAddress"); break(1)
        }
        val holdingAddress = CommandHelpers.refScriptHoldingAddress(network, sponsorAddress)
        Console.info("sponsor address", sponsorBech32)
        Console.info("holding address", holdingAddress.encode.getOrElse("<unencodable>"))
        println()

        if !CommandHelpers.canScanAddressUtxos(config, sponsorBech32) then {
            Console.error(
              "Migration cannot enumerate what to move: the reference-script scan needs a " +
                  "Blockfrost-API backend (cardano.backend = blockfrost, plus a project id or a " +
                  "cardano.blockfrost-url override)."
            )
            break(1)
        }

        // SOURCE scan: the sponsor WALLET only. This command enumerates what to move away from it;
        // refs already at the holding address are, by definition, done. (The exclusion scan below
        // is the union of both addresses — a different question: what must never be a fee input.)
        val sponsorItems = CommandHelpers.fetchAddressUtxos(config, sponsorBech32)
        val refs = CommandHelpers.parseRefScriptOutpoints(sponsorItems)

        // `fetchAddressUtxos` is best-effort: an HTTP failure returns the same empty Seq an empty
        // wallet does. Cross-check against the provider before believing "nothing to migrate", and
        // decide the three cases explicitly — a provider failure is NOT permission to continue,
        // because "raw scan empty AND provider unreachable" is a total backend outage, precisely
        // the state in which reporting "nothing to migrate" would be a false success.
        if sponsorItems.isEmpty then {
            val providerUtxos =
                provider.findUtxos(sponsorAddress).await(timeout).map(_.size).left.map(_.toString)
            emptyScanVerdict(providerUtxos) match {
                case EmptyScanVerdict.ProviderUnavailable(error) =>
                    // Includes NotFound: BlockfrostProvider answers an empty address with
                    // Right(empty), so a Left here is a real failure, never an empty wallet.
                    Console.error(
                      s"The address-utxos scan returned nothing and the provider could not be " +
                          s"queried either ($error) — the backend is down, so whether anything is " +
                          s"left to migrate is unknown. Check ${CommandHelpers.blockfrostBaseUrl(config)}."
                    )
                    break(1)
                case EmptyScanVerdict.ScanBlind(count) =>
                    Console.error(
                      s"The address-utxos scan returned nothing while the provider sees $count " +
                          s"UTxOs at the sponsor address — the scan is failing, the wallet is not " +
                          s"empty. Check ${CommandHelpers.blockfrostBaseUrl(config)}."
                    )
                    break(1)
                case EmptyScanVerdict.WalletEmpty =>
                    () // genuinely empty wallet — falls through to the "nothing to migrate" exit
            }
        }

        if refs.isEmpty then {
            // A backend whose `reference_script_hash` field is broken (it has regressed to always
            // null before) reports the same empty scan a finished migration does. Distinguish them
            // by shape: several ADA-only 50 ADA UTxOs at the wallet are what deploy-script-refs
            // makes, and nothing else does. Refuse to declare victory over a blind scan.
            val suspects = refShapedUtxoCount(sponsorItems)
            if suspects >= RefShapeSuspicionThreshold then {
                Console.error(
                  s"The scan found no reference scripts at the sponsor wallet, but $suspects of its " +
                      s"UTxOs have the reference-UTxO shape (ADA-only, exactly $RefUtxoLovelace " +
                      s"lovelace). That means the backend's `reference_script_hash` field is broken, " +
                      s"not that the migration is done. Re-run against a backend that populates it " +
                      s"(Dolos); migrating on a blind scan would leave refs at the wallet."
                )
                break(1)
            }
            Console.success("No reference-script UTxOs at the sponsor wallet — nothing to migrate")
            break(0)
        }

        Console.info("reference UTxOs to move", refs.size.toString)
        for (hash, outpoint) <- refs do Console.info(hash.toHex, show(outpoint))
        println()

        /** Move one ref UTxO. Every failure it anticipates comes back as a [[MoveOutcome]] so the
          * loop continues with the next ref. It can still throw: the `.await(timeout)` calls inside
          * throw on timeout, which aborts the whole loop and exits 1 through CliApp's handler. That
          * is acceptable — the command is idempotent, so a re-run picks up whatever is left.
          */
        def moveOne(hash: ScriptHash, outpoint: TransactionInput): MoveOutcome = {
            val label = s"${hash.toHex.take(12)}… ${show(outpoint)}"
            Console.step(0, s"Moving $label")

            val script = fetchScript(config, hash) match {
                case ScriptResolution.Resolved(s) => s
                case ScriptResolution.Unsupported(scriptType) =>
                    Console.warn(s"$label: script type '$scriptType' is not migrated — skipping")
                    return MoveOutcome.Skipped(
                      hash,
                      outpoint,
                      s"unsupported script type $scriptType"
                    )
                case ScriptResolution.Failed(reason) =>
                    Console.error(s"$label: $reason")
                    return MoveOutcome.Failed(hash, outpoint, reason)
            }

            val utxo = provider.findUtxo(outpoint).await(timeout) match {
                case Right(u) => u
                case Left(err) =>
                    Console.error(s"$label: resolving the UTxO: $err")
                    return MoveOutcome.Failed(hash, outpoint, s"UTxO lookup: $err")
            }

            // Fee correctness — the whole reason this command exists. TxBuilder prices the Conway
            // per-byte reference-script surcharge from the `scriptRef` of the UTxOs it is handed,
            // and a provider that drops it (or a backend whose reference_script_hash is null) would
            // hand over a bare output, under-estimate the fee and get FeeTooSmallUTxO. The script is
            // fetched and hash-verified, so this enrichment is exact, not a guess.
            val enriched = Utxo(
              utxo.input,
              TransactionOutput.Babbage(
                utxo.output.address,
                utxo.output.value,
                utxo.output.datumOption,
                Some(ScriptRef(script))
              )
            )
            // Same value, same script, new address: the move preserves the UTxO exactly.
            val newOutput = migratedOutput(holdingAddress, utxo.output.value, script)

            if dryRun then {
                Console.info(
                  s"would move ${hash.toHex}",
                  s"${show(outpoint)} -> holding (${utxo.output.value.coin.value} lovelace)"
                )
                return MoveOutcome.Planned(hash, outpoint)
            }

            // Fee/change pool: sponsor UTxOs minus every ref outpoint at either scanned address,
            // minus the one being moved (already in that set, kept explicit). Recomputed per tx so
            // a ref moved earlier in THIS run is excluded at its new location too. Spending a ref
            // for fees would destroy a deployed script; see DeployScriptRefsCommand.submitOne.
            val excludeInputs = CommandHelpers.refScriptOutpoints(
              config,
              CommandHelpers.refScriptScanAddresses(config, network, sponsorAddress)
            ) + outpoint
            val cleanUtxos = provider.findUtxos(sponsorAddress).await(timeout) match {
                // Belt and braces: drop anything the raw scan missed but the provider resolved a
                // scriptRef for. Either way, a UTxO carrying a reference script is never fee money.
                case Right(u) =>
                    u.filterNot { case (input, output) =>
                        excludeInputs.contains(input) || output.scriptRef.isDefined
                    }
                case Left(err) =>
                    Console.error(s"$label: fetching sponsor UTxOs: $err")
                    return MoveOutcome.Failed(hash, outpoint, s"sponsor UTxOs: $err")
            }
            if cleanUtxos.isEmpty then {
                Console.error(
                  s"$label: no clean sponsor UTxO left to pay the fee — top up $sponsorBech32"
                )
                return MoveOutcome.Failed(hash, outpoint, "no clean sponsor UTxO for fees")
            }

            val tx: Transaction =
                try
                    TxBuilder(provider.cardanoInfo)
                        .spend(enriched)
                        .addSteps(TransactionBuilderStep.Send(newOutput))
                        .complete(cleanUtxos, sponsorAddress)
                        .sign(signer)
                        .transaction
                catch {
                    case e: Exception =>
                        Console.error(s"$label build: ${e.getMessage}")
                        return MoveOutcome.Failed(hash, outpoint, s"build: ${e.getMessage}")
                }

            binocular.oracle.OracleTransactions.submitTx(provider, tx, timeout) match {
                case Left(err) =>
                    Console.error(s"$label submit: $err")
                    MoveOutcome.Failed(hash, outpoint, s"submit: $err")
                case Right(txHash) =>
                    // await window MUST exceed the poll budget (attempts*delayMs) or it preempts the
                    // poll and throws even when the tx confirms. See DeployBridgeCommand.confirmAwait.
                    val status = provider
                        .pollForConfirmation(
                          TransactionHash.fromHex(txHash),
                          maxAttempts = DeployBridgeCommand.ConfirmPollAttempts,
                          delayMs = DeployBridgeCommand.ConfirmPollDelayMs
                        )
                        .await(DeployBridgeCommand.confirmAwait)
                    status match {
                        case TransactionStatus.Confirmed =>
                            // Wait for the address-based UTxO index to catch up before the next
                            // move, so its fee/change selection doesn't pick inputs this tx just
                            // spent (pollForConfirmation checks tx status, not the address index;
                            // Blockfrost lags by a few slots between them). Wait on the HOLDING
                            // address: seeing the moved ref there proves the block was applied to
                            // the index, so the sponsor's change is visible and the next
                            // iteration's exclusion scan sees the ref at its new home.
                            binocular.oracle.OracleTransactions
                                .waitForUtxoAtAddress(
                                  provider,
                                  holdingAddress,
                                  TransactionHash.fromHex(txHash),
                                  timeout
                                ) match {
                                case Left(err) =>
                                    Console.error(s"$label UTxO-index wait: $err")
                                    MoveOutcome.Failed(hash, outpoint, s"UTxO-index wait: $err")
                                case Right(_) =>
                                    Console.success(
                                      s"moved: ${hash.toHex} ${show(outpoint)} -> $txHash#0"
                                    )
                                    MoveOutcome.Moved(hash, outpoint, txHash)
                            }
                        case other =>
                            Console.error(s"$label not confirmed: $other")
                            MoveOutcome.Failed(hash, outpoint, s"not confirmed: $other")
                    }
            }
        }

        // Serial, one tx per UTxO: each move must see the previous move's change before it selects.
        val outcomes = refs.map { case (hash, outpoint) => moveOne(hash, outpoint) }

        println()
        val moved = outcomes.collect { case m: MoveOutcome.Moved => m }
        val planned = outcomes.collect { case p: MoveOutcome.Planned => p }
        val skipped = outcomes.collect { case s: MoveOutcome.Skipped => s }
        val failed = outcomes.collect { case f: MoveOutcome.Failed => f }

        if dryRun then Console.info("would move", s"${planned.size}/${refs.size}")
        else Console.info("moved", s"${moved.size}/${refs.size}")
        if skipped.nonEmpty then {
            Console.warn(s"${skipped.size} skipped:")
            for s <- skipped do Console.info(s.hash.toHex, s.reason)
        }
        if failed.nonEmpty then {
            Console.error(s"${failed.size}/${refs.size} moves failed:")
            for f <- failed do Console.info(f.hash.toHex, s"${show(f.outpoint)}: ${f.reason}")
            break(1)
        }
        if dryRun then Console.success("Dry-run complete (resolved every move, submitted none)")
        else if skipped.nonEmpty then
            Console.success(
              s"Migration finished; ${skipped.size} UTxO(s) left at the wallet — see above"
            )
        else Console.success("All reference UTxOs now live at the native holding address")
        0
    }
}

object MigrateScriptRefsCommand {

    /** The lovelace `deploy-script-refs` ASKS for in every reference UTxO. Not what every ref
      * holds: at 4310 lovelace per byte, the largest scripts (fault_round1 ~12.8 KB, fault_round2
      * ~13.6 KB) get bumped above 50 ADA by the min-UTxO rule and hold their own higher value. The
      * shape guard matches only the exactly-50-ADA ones — that is >= 8 of the current refs, well
      * over [[RefShapeSuspicionThreshold]], so the guard still trips on a blind scan. Used only
      * there.
      */
    val RefUtxoLovelace: Long = 50_000_000L

    /** How many wallet UTxOs of the reference-UTxO shape make an empty scan implausible. Five is
      * the completion half's own script count, so a wallet that published even that half and then
      * scanned blind trips this instead of reporting "nothing to migrate".
      */
    val RefShapeSuspicionThreshold: Int = 5

    /** What happened to one reference UTxO. `Skipped` is deliberate (an unsupported script type),
      * `Failed` is not — only the latter makes the command exit non-zero.
      */
    enum MoveOutcome {
        case Moved(hash: ScriptHash, outpoint: TransactionInput, txHash: String)
        case Planned(hash: ScriptHash, outpoint: TransactionInput)
        case Skipped(hash: ScriptHash, outpoint: TransactionInput, reason: String)
        case Failed(hash: ScriptHash, outpoint: TransactionInput, reason: String)
    }

    /** What an EMPTY raw address-utxos scan means, cross-checked against the provider's independent
      * view of the same address. Only [[WalletEmpty]] lets the command carry on to report "nothing
      * to migrate"; the other two are fatal, because both mean the scan's emptiness proves nothing.
      */
    enum EmptyScanVerdict {
        case WalletEmpty
        case ScanBlind(providerUtxoCount: Int)
        case ProviderUnavailable(error: String)
    }

    /** Decide [[EmptyScanVerdict]] from the provider's UTxO count at the sponsor address (`Left` =
      * the provider query itself failed). Pure, so the "backend outage must not read as success"
      * rule is pinned by a unit test rather than by an operator noticing a wrong exit code.
      */
    def emptyScanVerdict(providerUtxos: Either[String, Int]): EmptyScanVerdict =
        providerUtxos match {
            case Left(error)       => EmptyScanVerdict.ProviderUnavailable(error)
            case Right(n) if n > 0 => EmptyScanVerdict.ScanBlind(n)
            case Right(_)          => EmptyScanVerdict.WalletEmpty
        }

    /** Outcome of rebuilding a script from a backend's bytes. */
    enum ScriptResolution {
        case Resolved(script: Script)
        case Unsupported(scriptType: String)
        case Failed(reason: String)
    }

    /** `TX_HASH#INDEX`. */
    private def show(outpoint: TransactionInput): String =
        s"${outpoint.transactionId.toHex}#${outpoint.index}"

    /** The recreated reference output: the source UTxO's full Value and the very same script, at
      * the holding address, with no datum. Pure, so the migration's one on-chain effect is pinned
      * by a unit test rather than only by a preprod run.
      */
    def migratedOutput(address: Address, value: Value, script: Script): TransactionOutput =
        TransactionOutput.Babbage(
          address,
          value,
          datumOption = None,
          scriptRef = Some(ScriptRef(script))
        )

    /** Strip one CBOR bytestring layer, or None if `bytes` is not a single CBOR bytestring. Some
      * backends hand out the script "double-encoded" (the CBOR of the CBOR the hash is taken over).
      */
    def cborUnwrapOnce(bytes: ByteString): Option[ByteString] =
        Try(ByteString.unsafeFromArray(Cbor.decode(bytes.bytes).to[Array[Byte]].value)).toOption

    /** Rebuild a script from a backend's `type` + `cbor` hex, and PROVE it is the right one: the
      * rebuilt script's hash must equal `expected`. Migration writes these bytes back on chain, so
      * an unverified rebuild would park a script the ref's consumers cannot use — silently, since
      * nothing else re-checks. Tries the bytes as given, then with one CBOR bytestring layer
      * removed (backends differ), then gives up loudly with both hashes.
      */
    def resolveScript(
        scriptType: String,
        cborHex: String,
        expected: ScriptHash
    ): ScriptResolution = {
        def mk(bytes: ByteString): Option[Script] = scriptType match {
            case "plutusV1" => Some(Script.PlutusV1(bytes))
            case "plutusV2" => Some(Script.PlutusV2(bytes))
            case "plutusV3" => Some(Script.PlutusV3(bytes))
            // A native script would have to be rebuilt from the /json endpoint's Timelock, and the
            // sponsor holds none: report it rather than pretend to migrate it.
            case _ => None
        }
        Try(ByteString.fromHex(cborHex)).toOption match {
            case None => ScriptResolution.Failed(s"script ${expected.toHex}: cbor is not hex")
            case Some(direct) =>
                mk(direct) match {
                    case None => ScriptResolution.Unsupported(scriptType)
                    case Some(asGiven) if asGiven.scriptHash == expected =>
                        ScriptResolution.Resolved(asGiven)
                    case Some(asGiven) =>
                        cborUnwrapOnce(direct).flatMap(mk) match {
                            case Some(unwrapped) if unwrapped.scriptHash == expected =>
                                ScriptResolution.Resolved(unwrapped)
                            case other =>
                                val unwrappedHash =
                                    other.map(_.scriptHash.toHex).getOrElse("<not CBOR-wrapped>")
                                ScriptResolution.Failed(
                                  s"rebuilt $scriptType script does not hash to the expected " +
                                      s"${expected.toHex}: as given it is ${asGiven.scriptHash.toHex}, " +
                                      s"CBOR-unwrapped it is $unwrappedHash"
                                )
                        }
                }
        }
    }

    /** How many of these address UTxOs look exactly like a `deploy-script-refs` output: ADA-only,
      * exactly [[RefUtxoLovelace]]. Nothing else in the sponsor wallet has that shape, so a nonzero
      * count next to an empty reference-script scan means the scan is blind. Pure.
      */
    def refShapedUtxoCount(items: Seq[ujson.Value]): Int =
        items.count { item =>
            item.obj.get("amount") match {
                case Some(ujson.Arr(amounts)) if amounts.sizeIs == 1 =>
                    val amount = amounts.head
                    amount.obj.get("unit").contains(ujson.Str("lovelace")) &&
                    lovelaceQuantity(amount).contains(RefUtxoLovelace)
                case _ => false
            }
        }

    /** `quantity` as a Long — a string on Blockfrost, a number on Yaci-style backends. */
    private def lovelaceQuantity(amount: ujson.Value): Option[Long] =
        amount.obj.get("quantity").flatMap {
            case ujson.Str(q) => q.toLongOption
            case ujson.Num(n) => Some(n.toLong)
            case _            => None
        }

    /** Fetch a script by hash from the configured Blockfrost-API backend (the same base URL and
      * `project_id` header the discovery scans use, so a self-hosted Dolos works) and rebuild it:
      * `/scripts/{hash}` for the type, `/scripts/{hash}/cbor` for the bytes. Verified by
      * [[resolveScript]] — never returns a script that does not hash to `hash`.
      */
    def fetchScript(config: BinocularConfig, hash: ScriptHash): ScriptResolution = {
        val base = CommandHelpers.blockfrostBaseUrl(config)
        val projectId = CommandHelpers.blockfrostProjectIdHeader(config)
        val hex = hash.toHex

        def get(path: String): Either[String, ujson.Value] =
            try {
                val client = java.net.http.HttpClient.newHttpClient()
                val req = java.net.http.HttpRequest
                    .newBuilder()
                    .uri(java.net.URI.create(s"$base$path"))
                    .header("project_id", projectId)
                    .GET()
                    .build()
                val resp = client.send(req, java.net.http.HttpResponse.BodyHandlers.ofString())
                if resp.statusCode() != 200 then
                    Left(s"GET $path -> HTTP ${resp.statusCode()}: ${resp.body().take(200)}")
                else Right(ujson.read(resp.body()))
            } catch { case e: Exception => Left(s"GET $path failed: ${e.getMessage}") }

        def str(json: ujson.Value, field: String, path: String): Either[String, String] =
            json.obj.get(field) match {
                case Some(ujson.Str(s)) => Right(s)
                case _                  => Left(s"$path has no string '$field' field")
            }

        val resolved = for {
            info <- get(s"/scripts/$hex")
            scriptType <- str(info, "type", s"/scripts/$hex")
            cborJson <- get(s"/scripts/$hex/cbor")
            cborHex <- str(cborJson, "cbor", s"/scripts/$hex/cbor")
        } yield resolveScript(scriptType, cborHex, hash)

        resolved.fold(ScriptResolution.Failed.apply, identity)
    }
}
