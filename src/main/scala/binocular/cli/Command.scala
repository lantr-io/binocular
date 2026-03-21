package binocular.cli

import binocular.*
import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, ScriptHash, TransactionHash, TransactionInput, Utxo, Value}
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.builtin.ByteString
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
    scriptAddress: Address,
    scriptAddressBech32: String,
    script: scalus.cardano.ledger.Script.PlutusV3,
    hdAccount: HdAccount,
    signer: TransactionSigner,
    sponsorAddress: Address,
    provider: BlockchainProvider
)

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
      * The NFT policy ID equals the script hash. Returns a failed Future if zero or multiple found.
      */
    def findOracleUtxo(
        provider: BlockchainProvider,
        nftPolicyId: ScriptHash
    )(using ExecutionContext): Future[Utxo] = {
        provider
            .queryUtxos(_.output.value.hasAsset(nftPolicyId, AssetName.empty))
            .execute()
            .flatMap {
                case Left(err) =>
                    Future.failed(new RuntimeException(s"Error fetching UTxOs: $err"))
                case Right(utxos) =>
                    assert(utxos.size == 1)
                    Future.successful(Utxo(utxos.head))
            }
    }

    /** Set up all the common oracle infrastructure (params, wallet, provider, script).
      *
      * Returns Left(error) on any setup failure.
      */
    def setupOracle(
        config: BinocularConfig
    )(using ExecutionContext): Either[String, OracleSetup] = {
        for {
            params <- config.oracle.toBitcoinValidatorParams()
            addrBech32 <- config.oracle.scriptAddress(config.cardano.cardanoNetwork)
            hdAccount <- config.wallet.createHdAccount()
            provider <- config.cardano.createBlockchainProvider()
        } yield {
            val signer = new TransactionSigner(Set(hdAccount.paymentKeyPair))
            val sponsorAddress = hdAccount.baseAddress(config.cardano.scalusNetwork)
            val scriptAddress = Address.fromBech32(addrBech32)
            val script = BitcoinContract.makeContract(params).script
            OracleSetup(
              params,
              scriptAddress,
              addrBech32,
              script,
              hdAccount,
              signer,
              sponsorAddress,
              provider
            )
        }
    }

    /** Find existing reference script UTxO (first match). */
    def findReferenceScriptUtxo(
        provider: BlockchainProvider,
        scriptAddress: Address,
        scriptHash: scalus.cardano.ledger.ScriptHash,
        timeout: Duration
    )(using ExecutionContext): Option[Utxo] = {
        val refs = OracleTransactions.findReferenceScriptUtxos(
          provider,
          scriptAddress,
          scriptHash,
          timeout
        )
        refs.headOption.flatMap { case (refHash, refIdx) =>
            val refInput = TransactionInput(TransactionHash.fromHex(refHash), refIdx)
            provider.findUtxo(refInput).await(timeout) match {
                case Right(u) => Some(u)
                case Left(_)  => None
            }
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
