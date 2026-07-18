package binocular.cli.commands

import binocular.*
import binocular.bitcoin.*
import binocular.oracle.*
import binocular.cli.{Command, CommandHelpers, Console}
import cats.syntax.either.*
import scalus.cardano.ledger.Utxo
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString
import scalus.utils.Hex.hexToBytes
import scalus.utils.await

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.{boundary, Try}
import scala.util.boundary.break

/** Owner state reset of a stale oracle (`OracleAction.SetState`).
  *
  * Replaces the oracle's ChainState in ONE transaction while preserving the NFT and every
  * downstream contract parameterized by the oracle script hash — the recovery path for a deep reorg
  * past the maturation depth (where the append-only `confirmedBlocksRoot` commits to orphaned
  * blocks and can never be repaired by `UpdateOracle`).
  *
  * The replacement state anchors at `--height H` (like `init --start-block H`), but its
  * confirmed-blocks MPF is rebuilt from the configured `oracle.start-height` up to `H` against this
  * bitcoind's canonical chain, so everything previously provable stays provable and `start-height`
  * needs no change. Without a configured `start-height` the root contains only the anchor block
  * (init semantics) and `start-height` MUST then be set to `H`.
  *
  * Allowed on-chain only when the oracle is stale (same predicate as CloseOracle) and the tx is
  * signed by the owner — see the security argument in the Whitepaper §Owner State Reset.
  */
case class SetStateCommand(height: Long, dryRun: Boolean = false) extends Command {

    override def execute(config: BinocularConfig): Int = {
        Console.header("Binocular Oracle SetState (owner state reset)")
        if dryRun then Console.warn("Dry-run mode — will build the state but not submit")
        println()
        runSetState(config)
    }

    private def runSetState(config: BinocularConfig): Int = boundary {
        given ec: ExecutionContext = binocular.cli.DaemonExecution.ec
        val timeout = config.oracle.transactionTimeout.seconds

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err); break(1)
        }

        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        val info =
            try rpc.getBlockchainInfo().await(30.seconds)
            catch {
                case e: Exception =>
                    Console.error(s"Bitcoin RPC: ${e.getMessage}"); break(1)
            }
        Console.info("Bitcoin", s"${config.bitcoinNode.url} (${info.chain}, tip=${info.blocks})")
        Console.info("Cardano", config.cardano.network)
        Console.info("Oracle", setup.scriptAddressBech32)
        Console.info("Target height", f"$height%,d")
        if height > info.blocks then
            Console.error(s"Target height $height is beyond the Bitcoin tip ${info.blocks}")
            break(1)

        // Current oracle UTxO + staleness pre-flight (replicates the validator's rule so a
        // doomed tx is never paid for; same approach as CloseCommand).
        val oracleUtxo: Utxo =
            try
                CommandHelpers
                    .findOracleUtxo(setup.provider, setup.script.scriptHash)
                    .await(timeout)
            catch {
                case e: Exception => Console.error(e.getMessage); break(1)
            }
        Console.info(
          "Oracle UTxO",
          s"${oracleUtxo.input.transactionId.toHex}#${oracleUtxo.input.index}"
        )
        val currentState = CommandHelpers.parseChainState(oracleUtxo).getOrElse {
            Console.error("Oracle UTxO has no valid ChainState datum"); break(1)
        }
        val headTs = currentState.ctx.timestamps.head.toLong
        val (_, intervalEndSeconds) =
            OracleTransactions.computeValidityIntervalTime(setup.provider.cardanoInfo)
        val gap = intervalEndSeconds.toLong - headTs
        val limit = setup.params.closureTimeout.toLong
        Console.info(
          "Staleness",
          s"last confirmed ts $headTs (${Instant.ofEpochSecond(headTs)}), " +
              s"age ${gap}s / ${limit}s — ${
                      if gap > limit then "stale (can reset)" else "fresh (cannot reset)"
                  }"
        )
        if gap <= limit then
            Console.error(
              "Oracle is not stale. SetState is only allowed once no confirmed-block timestamp " +
                  "is within the closure-timeout window."
            )
            break(1)

        // Replacement state: anchor at `height`, MPF covering start-height..height (canonical).
        Console.log(s"Building replacement state anchored at height $height...")
        val anchorState =
            try BitcoinChainState.getInitialChainState(rpc, height.toInt).await(60.seconds)
            catch {
                case e: Exception =>
                    Console.error(s"Fetching Bitcoin state at $height: ${e.getMessage}"); break(1)
            }
        val newState = config.oracle.startHeight match {
            case Some(start) if start <= height =>
                Console.log(
                  s"Rebuilding confirmed-blocks MPF from $start..$height (canonical chain)..."
                )
                // Dedicated timeout scaled to the range — the tx timeout (120s) would fail a
                // mainnet-scale recovery of thousands of sequential getblockhash round-trips.
                val mpfTimeout = (60 + (height - start + 1) / 5).seconds
                val root = buildCanonicalMpf(rpc, start, height, mpfTimeout).valueOr { err =>
                    Console.error(err); break(1)
                }
                Console.logSuccess(
                  s"MPF rebuilt: ${height - start + 1} confirmed blocks, root ${root.rootHash.toHex}"
                )
                anchorState.copy(confirmedBlocksRoot = root.rootHash)
            case Some(start) =>
                Console.error(
                  s"Configured start-height $start is above target height $height — pick a " +
                      "target at or above it"
                )
                break(1)
            case scala.None =>
                Console.warn(
                  s"No oracle.start-height configured — the new root contains ONLY block $height " +
                      s"(init semantics). Set start-height = $height in the config afterwards, or " +
                      "MPF reconstruction will fail on the next promotion."
                )
                anchorState
        }
        Console.info("New height", newState.ctx.height)
        Console.info("New tip", newState.ctx.lastBlockHash.toHex)
        Console.info("New root", newState.confirmedBlocksRoot.toHex)

        if dryRun then {
            Console.success("Dry-run complete. SetState transaction not submitted.")
            break(0)
        }

        val referenceScriptUtxo = CommandHelpers.findReferenceScriptUtxo(
          setup.provider,
          setup.scriptAddress,
          setup.script,
          timeout
        ) match {
            case Some(utxo) => utxo
            case None =>
                Console.error("Reference script not found. Run 'binocular init' first.")
                break(1)
        }

        // Keep the bridge's CIP-33 reference-script UTxOs (parked at the sponsor wallet) OUT of fee
        // selection — BlockfrostProvider drops their scriptRef, so picking one under-estimates the
        // fee by the Conway ref-script surcharge → FeeTooSmallUTxO. Same fix the update loop applies.
        val excludeInputs =
            CommandHelpers.refScriptOutpoints(config, setup.sponsorAddress.encode.getOrElse(""))
        if excludeInputs.nonEmpty then
            Console.info(
              "Excluding",
              s"${excludeInputs.size} ref-script UTxO(s) from fee selection"
            )

        Console.log("Building SetState transaction...")
        OracleTransactions.buildAndSubmitSetStateTransaction(
          setup.provider,
          setup.hdAccount,
          oracleUtxo,
          referenceScriptUtxo,
          setup.script,
          newState,
          timeout,
          excludeInputs
        ) match {
            case Right(txHash) =>
                Console.logSuccess(s"Oracle state reset | tx: $txHash")
                Console.info("New oracle UTxO", s"$txHash#0")
                0
            case Left(err) =>
                Console.error(s"Failed to set oracle state: $err")
                1
        }
    }

    /** Fold the canonical block hashes `start..end` into a fresh MPF (key = value = LE hash),
      * mirroring `CommandHelpers.rebuildMpf` but without an expected root (this BUILDS the new
      * commitment rather than verifying an old one).
      */
    private def buildCanonicalMpf(
        rpc: SimpleBitcoinRpc,
        start: Long,
        end: Long,
        timeout: Duration
    )(using ExecutionContext): Either[String, OffChainMPF] = {
        def loop(heights: scala.List[Long], mpf: OffChainMPF): Future[OffChainMPF] =
            heights match {
                case scala.Nil => Future.successful(mpf)
                case h :: tail =>
                    for {
                        hashHex <- rpc.getBlockHash(h.toInt)
                        blockHash = ByteString.fromArray(hashHex.hexToBytes.reverse)
                        result <- loop(tail, mpf.insert(blockHash, blockHash))
                    } yield result
            }
        Try(loop((start to end).toList, OffChainMPF.empty).await(timeout)).toEither
            .leftMap(e => s"Rebuilding MPF from $start..$end: ${e.getMessage}")
    }
}
