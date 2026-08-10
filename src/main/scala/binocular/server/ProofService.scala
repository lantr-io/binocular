package binocular.server

import binocular.BinocularConfig
import binocular.bitcoin.SimpleBitcoinRpc
import binocular.cli.CommandHelpers
import binocular.oracle.{BitcoinContract, ChainState}
import binocular.server.ProofApi.ApiError
import binocular.watchtower.{BridgeState, ConfigDatum, CpoHistorySource, PegInProofBundle, ProviderChainHistory, SweptPegInsProofService, TreasuryMovementValidator}

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{AssetName, Credential, ScriptHash, Utxo}
import scalus.cardano.node.BlockchainProvider
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.ByteString

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.Try
import scalus.utils.await

/** The proof server's I/O layer: resolve live chain state, cache the two derived tries by the roots
  * that identify them, and delegate proving and shaping to the tested services and [[ProofApi]].
  *
  * Serves both transports: the HTTP endpoints ([[ProofServer]]) and the manual CLI commands
  * (`spi-proof`, `deposit-proof`) call the same two methods, so they cannot drift apart.
  *
  * ==Caching==
  *
  * Both proofs are served from a derived structure that is expensive to build and identified
  * exactly by a root hash:
  *
  *   - the SPI trie, keyed by the singleton's `(spi_root, treasury_utxo_id)`: rebuilt only when a
  *     TM Confirm advances the singleton;
  *   - the oracle's confirmed-blocks MPF, keyed by `confirmed_blocks_root`: rebuilt only when the
  *     oracle promotes blocks.
  *
  * The keys make staleness structurally impossible: a cached trie is used only when the CURRENT
  * on-chain root equals the root the trie was reconciled against, so a cache hit proves exactly
  * what a rebuild would.
  */
final class ProofService(
    provider: BlockchainProvider,
    history: CpoHistorySource,
    rpc: SimpleBitcoinRpc,
    network: Network,
    oraclePolicyId: ScriptHash,
    configNftPolicy: ScriptHash,
    configNftAsset: AssetName,
    oracleStartHeight: Option[Long],
    timeout: Duration,
    log: String => Unit = _ => ()
)(using ExecutionContext) {

    private val spiTrieCache = new AtomicReference[Option[(String, OffChainMPF)]](None)
    private val blocksMpfCache = new AtomicReference[Option[(ByteString, OffChainMPF)]](None)

    /** Serve the [CPI-9] swept-peg-ins membership proof for one outpoint, as JSON ([SPI-4]). */
    def spiProof(outpointArg: String): Either[ApiError, String] =
        for {
            outpoint <- ProofApi
                .parseBtcOutpoint(outpointArg)
                .left
                .map(ProofApi.invalidOutpoint)
            resolved <- resolveBridge()
            (state, tmAddressBech32) = resolved
            trie <- spiTrie(state, tmAddressBech32)
            proof <- SweptPegInsProofService
                .proveFrom(trie, outpoint)
                .left
                .map(ProofApi.spiError)
        } yield ProofApi.spiProofJson(proof)

    /** Serve the [OB-12] deposit-inclusion bundle for one outpoint, as JSON ([OB-13]). */
    def depositProof(outpointArg: String): Either[ApiError, String] =
        for {
            outpoint <- ProofApi
                .parseBtcOutpoint(outpointArg)
                .left
                .map(ProofApi.invalidOutpoint)
            chainState <- oracleState()
            mpf <- confirmedBlocksMpf(chainState)
            bundle <- backend("building the deposit bundle") {
                PegInProofBundle.produceForOutpoint(rpc, mpf, outpoint).await(timeout)
            }.flatMap(_.left.map(ProofApi.depositError))
        } yield ProofApi.depositBundleJson(bundle)

    /** A one-pass construction check for `--dry-run`: resolve both proof sources without serving.
      */
    def dryRunCheck(): Either[ApiError, Unit] =
        for {
            resolved <- resolveBridge()
            _ <- spiTrie(resolved._1, resolved._2)
            chainState <- oracleState()
            _ <- confirmedBlocksMpf(chainState)
        } yield ()

    // --- bridge state singleton ------------------------------------------------------------------

    /** The singleton's `BridgeState` and the TM address (Config field 4, [CFG-2]). */
    private def resolveBridge(): Either[ApiError, (BridgeState, String)] =
        for {
            configUtxo <- findByNft(
              configNftPolicy,
              configNftAsset,
              what = "config UTxO",
              code = "config_missing"
            )
            config <- configUtxo.output.inlineDatum
                .flatMap(d => Try(d.to[ConfigDatum]).toOption)
                .toRight(
                  ApiError(
                    503,
                    "config_malformed",
                    "the config UTxO's datum does not decode as the rev-5.4 ConfigDatum"
                  )
                )
            singletonUtxo <- findByNft(
              ScriptHash.fromHex(config.bridgeStatePolicy.toHex),
              AssetName(TreasuryMovementValidator.BridgeStateAssetName),
              what = "bridge state singleton",
              code = "singleton_missing",
              hint = " — run bootstrap-bridge-state (or deploy-bridge) and publish its policy in " +
                  "Config field 3"
            )
            state <- singletonUtxo.output.inlineDatum
                .flatMap(d => Try(d.to[BridgeState]).toOption)
                .toRight(
                  ApiError(
                    503,
                    "singleton_malformed",
                    "the singleton UTxO's datum does not decode as the 4-field BridgeState — " +
                        "refusing to guess at a root ([LIB-1])"
                  )
                )
            tmAddress <- Address(
              network,
              Credential.ScriptHash(ScriptHash.fromHex(config.tmScriptHash.toHex))
            ).encode.toOption
                .toRight(
                  ApiError(
                    503,
                    "config_malformed",
                    s"cannot encode the TM address for script hash ${config.tmScriptHash.toHex}"
                  )
                )
        } yield (state, tmAddress)

    /** The reconciled SPI trie for `state`, cached by `(spi_root, head)`. */
    private def spiTrie(
        state: BridgeState,
        tmAddressBech32: String
    ): Either[ApiError, OffChainMPF] = {
        val key = state.spiRoot.toHex + state.treasuryUtxoId.toHex
        spiTrieCache.get() match {
            case Some((cached, trie)) if cached == key => Right(trie)
            case _ =>
                for {
                    fetch <- SweptPegInsProofService
                        .cardanoFetcher(history, tmAddressBech32, log)
                        .left
                        .map(err => ApiError(503, "history_unavailable", err))
                    trie <- SweptPegInsProofService
                        .confirmedTrie(state.spiRoot, state.treasuryUtxoId, fetch)
                        .left
                        .map(ProofApi.spiError)
                } yield {
                    log(
                      s"spi trie rebuilt for spi_root ${state.spiRoot.toHex} " +
                          s"(head ${state.treasuryUtxoId.toHex})"
                    )
                    spiTrieCache.set(Some((key, trie)))
                    trie
                }
        }
    }

    // --- oracle ----------------------------------------------------------------------------------

    private def oracleState(): Either[ApiError, ChainState] =
        backend("reading the oracle UTxO") {
            CommandHelpers.findOracleUtxo(provider, oraclePolicyId).await(timeout)
        }.flatMap(utxo =>
            CommandHelpers
                .parseChainState(utxo)
                .toRight(
                  ApiError(503, "oracle_malformed", "oracle UTxO has no valid ChainState datum")
                )
        )

    /** The oracle's confirmed-blocks MPF mirror, cached by `confirmed_blocks_root`. */
    private def confirmedBlocksMpf(chainState: ChainState): Either[ApiError, OffChainMPF] = {
        val root = chainState.confirmedBlocksRoot
        blocksMpfCache.get() match {
            case Some((cached, mpf)) if cached == root => Right(mpf)
            case _ =>
                backend("rebuilding the confirmed-blocks MPF") {
                    CommandHelpers.reconstructMpf(rpc, chainState, oracleStartHeight)
                }.flatMap(_.left.map(err => ApiError(503, "oracle_mpf_unavailable", err)))
                    .map { mpf =>
                        log(s"confirmed-blocks MPF rebuilt for root ${root.toHex}")
                        blocksMpfCache.set(Some((root, mpf)))
                        mpf
                    }
        }
    }

    // --- plumbing --------------------------------------------------------------------------------

    private def findByNft(
        policy: ScriptHash,
        asset: AssetName,
        what: String,
        code: String,
        hint: String = ""
    ): Either[ApiError, Utxo] = {
        val address = Address(network, Credential.ScriptHash(policy))
        backend(s"fetching the $what") {
            provider.findUtxos(address).await(timeout)
        }.flatMap {
            case Left(err) => Left(ApiError(503, "backend_error", s"fetching the $what: $err"))
            case Right(utxos) =>
                utxos
                    .collectFirst {
                        case (in, out) if out.value.hasAsset(policy, asset) => Utxo(in, out)
                    }
                    .toRight(
                      ApiError(
                        503,
                        code,
                        s"no UTxO carrying the ${policy.toHex}/${asset.bytes.toHex} NFT " +
                            s"at $address$hint"
                      )
                    )
        }
    }

    /** Run one backend interaction, turning any thrown exception into a 503 rather than a crash: a
      * proof server answering arbitrary callers must never die on a provider hiccup.
      */
    private def backend[A](what: String)(body: => A): Either[ApiError, A] =
        try Right(body)
        catch {
            case e: Throwable =>
                Left(ApiError(503, "backend_error", s"$what: ${e.getMessage}"))
        }
}

object ProofService {

    /** Wire a [[ProofService]] from the loaded configuration — the one construction path both the
      * watchtower worker and the standalone `serve-proofs` command use. No wallet is needed:
      * serving proofs signs nothing.
      */
    def fromConfig(
        config: BinocularConfig,
        log: String => Unit
    )(using ExecutionContext): Either[String, ProofService] = {
        val timeout = config.oracle.transactionTimeout.seconds
        for {
            params <- config.oracle.toBitcoinValidatorParams(config.bitcoinNode.bitcoinNetwork)
            provider <- config.cardano.createBlockchainProvider()
            history <- ProviderChainHistory.from(provider, timeout)
        } yield new ProofService(
          provider = provider,
          history = history,
          rpc = new SimpleBitcoinRpc(config.bitcoinNode),
          network = config.cardano.scalusNetwork,
          oraclePolicyId = BitcoinContract.script(params).scriptHash,
          configNftPolicy = ScriptHash.fromHex(config.bridge.configNftPolicyId),
          configNftAsset = AssetName(ByteString.fromHex(config.bridge.configNftAssetName)),
          oracleStartHeight = config.oracle.startHeight,
          timeout = timeout,
          log = log
        )
    }
}
