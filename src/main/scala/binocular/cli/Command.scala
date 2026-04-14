package binocular.cli

import binocular.*
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{AssetName, ScriptHash, ScriptRef, TransactionOutput, Utxo, Value}
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.fromData
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.utils.Hex.hexToBytes

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.Try
import scalus.utils.await

/** Base trait for all CLI commands
  *
  * Each command implements this trait and provides its execution logic. Commands receive a
  * BinocularConfig loaded at startup.
  */
trait Command {

    /** Execute the command
      *
      * @param config
      *   The loaded BinocularConfig
      * @return
      *   Exit code (0 for success, non-zero for error)
      */
    def execute(config: BinocularConfig): Int
}

/** Common setup context for commands that interact with the oracle on-chain */
case class OracleSetup(
    params: BitcoinValidatorParams,
    compiled: PlutusV3[Data => Unit],
    hdAccount: HdAccount,
    provider: BlockchainProvider,
    network: Network
) {
    lazy val script = compiled.script
    lazy val scriptAddress = compiled.address(network)
    lazy val scriptAddressBech32 = scriptAddress.encode.get
    lazy val signer = hdAccount.signerForUtxos
    lazy val sponsorAddress = hdAccount.baseAddress(network)
}

/** Represents a validated oracle UTxO with its parsed ChainState */
case class ValidOracleUtxo(
    utxo: Utxo,
    chainState: ChainState
) {
    def txHash: String = utxo.input.transactionId.toHex
    def outputIndex: Int = utxo.input.index
    def utxoRef: String = s"$txHash:$outputIndex"
}

/** Helper utilities for commands */
object CommandHelpers {

    /** Print BitcoinValidatorParams as an aligned summary via Console.info, suitable for use in
      * command startup banners. Field order matches BitcoinValidatorParams declaration so the
      * on-chain parameter set is easy to scan at a glance.
      */
    def printParams(params: BitcoinValidatorParams): Unit = {
        def humanDuration(seconds: BigInt): String = {
            val s = seconds.toLong
            if s < 60 then s"${s}s"
            else if s < 3600 then f"${s / 60.0}%.1fm"
            else if s < 86400 then f"${s / 3600.0}%.2fh"
            else f"${s / 86400.0}%.2fd"
        }
        Console.info(
          "oneShotTxOutRef",
          s"${params.oneShotTxOutRef.id.hash.toHex}#${params.oneShotTxOutRef.idx}"
        )
        Console.info("owner", params.owner.hash.toHex)
        Console.info("powLimit", f"0x${params.powLimit.toString(16)}")
        Console.info("allowMinDifficultyBlocks", params.allowMinDifficultyBlocks)
        Console.info("maturationConfirmations", params.maturationConfirmations)
        Console.info(
          "challengeAging",
          s"${params.challengeAging}s (${humanDuration(params.challengeAging)})"
        )
        Console.info(
          "closureTimeout",
          s"${params.closureTimeout}s (${humanDuration(params.closureTimeout)})"
        )
        Console.info("maxBlocksInForkTree", params.maxBlocksInForkTree)
        Console.info("testingMode", params.testingMode)
    }

    /** Parse UTxO reference string (TX_HASH:OUTPUT_INDEX) */
    def parseUtxo(utxo: String): Either[String, (String, Int)] = {
        val parts = utxo.split(":")
        if parts.length != 2 then {
            Left(s"Invalid UTxO format. Expected: <TX_HASH>:<OUTPUT_INDEX>")
        } else {
            parts(1).toIntOption match {
                case Some(index) => Right((parts(0), index))
                case None        => Left(s"Invalid output index: ${parts(1)}")
            }
        }
    }

    /** Check if a UTxO is an oracle UTxO (has inline datum, no reference script) */
    def isOracleUtxo(utxo: Utxo): Boolean =
        utxo.output.inlineDatum.isDefined && utxo.output.scriptRef.isEmpty

    /** Try to parse ChainState from UTxO's inline datum */
    def parseChainState(utxo: Utxo): Option[ChainState] =
        utxo.output.inlineDatum.flatMap { data =>
            Try {
                fromData[ChainState](data)
            }.toOption
        }

    /** Check if ChainState is valid (has 11 sorted timestamps) */
    def isValidChainState(chainState: ChainState): Boolean = {
        val timestamps = chainState.ctx.timestamps.toScalaList
        timestamps.size >= 11 && timestamps.sliding(2).forall {
            case Seq(a, b) => a >= b
            case _         => true
        }
    }

    /** Try to get a valid oracle UTxO from a raw UTxO */
    def tryValidateOracleUtxo(utxo: Utxo): Option[ValidOracleUtxo] =
        if !isOracleUtxo(utxo) then None
        else
            parseChainState(utxo).filter(isValidChainState).map { chainState =>
                ValidOracleUtxo(utxo, chainState)
            }

    /** Filter list of UTxOs to only valid oracle UTxOs */
    def filterValidOracleUtxos(utxos: List[Utxo]): List[ValidOracleUtxo] =
        utxos.flatMap(tryValidateOracleUtxo)

    /** Find the oracle UTxO by looking for the NFT with the given policy ID.
      *
      * The NFT policy ID equals the script hash. The script address is derived from the policy ID
      * and the provider's network. Returns a failed Future if zero or multiple found.
      */
    def findOracleUtxo(
        provider: BlockchainProvider,
        nftPolicyId: ScriptHash
    )(using ExecutionContext): Future[Utxo] = {
        val scriptAddress =
            Address(
              provider.cardanoInfo.network,
              scalus.cardano.ledger.Credential.ScriptHash(nftPolicyId)
            )
        provider.findUtxos(scriptAddress).flatMap {
            case Left(err) =>
                Future.failed(new RuntimeException(s"Error fetching UTxOs: $err"))
            case Right(utxos) =>
                val matches = utxos.collect {
                    case (input, output) if output.value.hasAsset(nftPolicyId, AssetName.empty) =>
                        Utxo(input, output)
                }.toList
                matches match {
                    case List(utxo) => Future.successful(utxo)
                    case Nil =>
                        Future.failed(
                          new RuntimeException(
                            s"No oracle UTxO with NFT $nftPolicyId found"
                          )
                        )
                    case multiple =>
                        Future.failed(
                          new RuntimeException(
                            s"Multiple oracle UTxOs (${multiple.size}) with NFT $nftPolicyId found"
                          )
                        )
                }
        }
    }

    /** Set up all the common oracle infrastructure (params, wallet, provider, compiled contract).
      *
      * Returns Left(error) on any setup failure.
      */
    def setupOracle(
        config: BinocularConfig
    )(using ExecutionContext): Either[String, OracleSetup] = {
        for {
            params <- config.oracle.toBitcoinValidatorParams(config.bitcoinNode.bitcoinNetwork)
            hdAccount <- config.wallet.createHdAccount()
            provider <- config.cardano.createBlockchainProvider()
        } yield {
            val compiled = BitcoinContract.makeContract(params)
            OracleSetup(
              params,
              compiled,
              hdAccount,
              provider,
              config.cardano.scalusNetwork
            )
        }
    }

    /** Find existing reference script UTxO at the script address.
      *
      * The provider does not populate scriptRef when listing UTxOs, so we identify the reference
      * script UTxO as the one at the script address that does NOT carry the oracle NFT (the NFT
      * policy ID equals the script hash). We then reconstruct the output with the known script.
      */
    def findReferenceScriptUtxo(
        provider: BlockchainProvider,
        scriptAddress: Address,
        script: scalus.cardano.ledger.Script,
        timeout: Duration
    )(using ExecutionContext): Option[Utxo] = {
        val nftPolicyId = script.scriptHash
        provider.findUtxos(scriptAddress).await(timeout) match {
            case Right(utxos) =>
                utxos.toList
                    .find { case (_, output) =>
                        !output.value.hasAsset(nftPolicyId, AssetName.empty)
                    }
                    .map { case (input, output) =>
                        Utxo(
                          input,
                          TransactionOutput(
                            address = output.address,
                            value = output.value,
                            datumOption = output.datumOption,
                            scriptRef = Some(ScriptRef(script))
                          )
                        )
                    }
            case Left(_) => None
        }
    }

    /** Reconstruct off-chain MPF from Bitcoin RPC by re-inserting all confirmed block hashes. */
    def rebuildMpf(
        rpc: SimpleBitcoinRpc,
        startHeight: Long,
        endHeight: Long,
        expectedRoot: ByteString
    )(using ExecutionContext): Either[String, OffChainMPF] = {
        def loop(heights: List[Long], mpf: OffChainMPF): Future[OffChainMPF] = {
            heights match {
                case Nil => Future.successful(mpf)
                case h :: tail =>
                    for {
                        hashHex <- rpc.getBlockHash(h.toInt)
                        blockHash = ByteString.fromArray(hashHex.hexToBytes.reverse)
                        updatedMpf = mpf.insert(blockHash, blockHash)
                        result <- loop(tail, updatedMpf)
                    } yield result
            }
        }
        val heights = (startHeight to endHeight).toList
        try {
            val rebuilt = loop(heights, OffChainMPF.empty).await(120.seconds)
            if rebuilt.rootHash != expectedRoot then
                Left(
                  s"Rebuilt MPF root does not match on-chain confirmedBlocksRoot. " +
                      s"Expected: ${expectedRoot.toHex}, got: ${rebuilt.rootHash.toHex}"
                )
            else Right(rebuilt)
        } catch {
            case e: Exception => Left(s"Error rebuilding MPF: ${e.getMessage}")
        }
    }

    /** Detect a Bitcoin reorg and compute the correct parentPath and block range.
      *
      * Compares the oracle's fork tree tip against Bitcoin's canonical chain. If they diverge,
      * walks backwards from the oracle tip to find the common ancestor, then returns the correct
      * parentPath and start height for submitting the canonical chain's blocks.
      *
      * @return
      *   `(parentPath, startHeight)` — either the tip path with `highestKnown + 1` (no reorg) or
      *   the path to the common ancestor with `ancestor + 1` (reorg detected).
      */
    def detectReorgAndComputePath(
        rpc: BitcoinRpc,
        chainState: ChainState
    )(using ExecutionContext): (scalus.cardano.onchain.plutus.prelude.List[BigInt], Long) = {
        import scalus.cardano.onchain.plutus.prelude.List as ScalusList
        val confirmedHeight = chainState.ctx.height.toLong
        val forkTree = chainState.forkTree

        if forkTree == ForkTree.End then
            // Empty fork tree — parent is confirmed tip, start from confirmed + 1
            (ScalusList.Nil, confirmedHeight + 1)
        else
            val highestKnown = forkTree.highestHeight(chainState.ctx.height).toLong

            // Get the hash of the best chain tip in the fork tree
            forkTree.bestChainTipHash match
                case scala.None =>
                    (forkTree.findTipPath, highestKnown + 1)
                case scala.Some(oracleTipHash) =>
                    // Get Bitcoin's canonical block hash at the same height
                    val canonicalHashHex =
                        rpc.getBlockHash(highestKnown.toInt).await(30.seconds)
                    val canonicalHash =
                        ByteString.fromArray(canonicalHashHex.hexToBytes.reverse)

                    if oracleTipHash == canonicalHash then
                        // No reorg — oracle tip matches Bitcoin canonical chain
                        (forkTree.findTipPath, highestKnown + 1)
                    else
                        // Reorg detected! Walk backwards to find common ancestor
                        Console.logWarn(
                          s"Reorg detected at height $highestKnown: " +
                              s"oracle=${oracleTipHash.toHex}, canonical=${canonicalHash.toHex}"
                        )

                        var searchHeight = highestKnown - 1
                        var ancestorFound = false
                        var ancestorHeight = confirmedHeight
                        var ancestorHash: ByteString = chainState.ctx.lastBlockHash

                        while !ancestorFound && searchHeight > confirmedHeight do {
                            val hashHex =
                                rpc.getBlockHash(searchHeight.toInt).await(30.seconds)
                            val hash =
                                ByteString.fromArray(hashHex.hexToBytes.reverse)

                            if forkTree.existsHash(hash) then {
                                ancestorFound = true
                                ancestorHeight = searchHeight
                                ancestorHash = hash
                            } else searchHeight -= 1
                        }

                        val parentPath = if ancestorHeight == confirmedHeight then {
                            Console.logWarn(
                              s"Common ancestor is confirmed tip at height $confirmedHeight"
                            )
                            ScalusList.Nil
                        } else {
                            Console.logWarn(
                              s"Common ancestor found at height $ancestorHeight"
                            )
                            forkTree.findPathToHash(ancestorHash).getOrElse {
                                throw new RuntimeException(
                                  s"Bug: existsHash found $ancestorHeight but findPathToHash failed"
                                )
                            }
                        }

                        (parentPath, ancestorHeight + 1)
    }

    /** Reconstruct off-chain MPF, checking first if the state has only a single block. */
    def reconstructMpf(
        rpc: SimpleBitcoinRpc,
        chainState: ChainState,
        startHeight: Option[Long]
    )(using ExecutionContext): Either[String, OffChainMPF] = {
        val initialMpf =
            OffChainMPF.empty.insert(chainState.ctx.lastBlockHash, chainState.ctx.lastBlockHash)
        if initialMpf.rootHash == chainState.confirmedBlocksRoot then Right(initialMpf)
        else
            startHeight match {
                case None =>
                    Left(
                      "Previous promotions detected but ORACLE_START_HEIGHT not configured"
                    )
                case Some(h) =>
                    rebuildMpf(
                      rpc,
                      h,
                      chainState.ctx.height.toLong,
                      chainState.confirmedBlocksRoot
                    )
            }
    }
}
