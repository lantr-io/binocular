package binocular.cli

import binocular.*
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{AssetName, ScriptHash, ScriptRef, TransactionHash, TransactionInput, TransactionOutput, Utxo, Value}
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
            params <- config.oracle.toBitcoinValidatorParams()
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

    /** Find existing reference script UTxO.
      *
      * The provider may not populate scriptRef when listing UTxOs, so we search for UTxOs at the
      * script address that have no inline datum (the oracle UTxO has inline datum, the reference
      * script UTxO does not) and reconstruct the output with the known script attached.
      */
    def findReferenceScriptUtxo(
        provider: BlockchainProvider,
        scriptAddress: Address,
        script: scalus.cardano.ledger.Script,
        timeout: Duration
    )(using ExecutionContext): Option[Utxo] = {
        provider.findUtxos(scriptAddress).await(timeout) match {
            case Right(utxos) =>
                utxos.toList
                    .find { case (_, output) =>
                        output.inlineDatum.isEmpty && output.scriptRef.isEmpty
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
