package binocular.watchtower

import binocular.server.ProofService.SweptSnapshot
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.utils.await
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.Try

/** Shared completion preparation. Reference discovery and building are separate so a depositor can
  * obtain the signing message without a signature, funding, or deployed reference scripts.
  */
final case class PegInCompletion(
    pegIn: PegInContract,
    completedPegIns: CompletedPegInsContract,
    bridgedToken: BridgedTokenContract,
    tokenAsset: AssetName
) {
    import PegInCompletion.*
    private val cpiAsset = AssetName(CompletedPegInsContract.assetName)

    def prepare(
        provider: BlockchainProvider,
        history: CpoHistorySource,
        snapshot: SweptSnapshot,
        pirInput: TransactionInput,
        recipient: Address,
        timeout: Duration,
        priorPegins: Seq[ByteString] = Nil
    )(using ExecutionContext): Either[String, Prepared] = Try {
        val network = provider.cardanoInfo.network
        val ctx = snapshot.context
        require(
          ctx.config.pegInScriptHash.toHex == pegIn.policyId.toHex &&
              ctx.config.completedPegInsPolicy.toHex == completedPegIns.policyId.toHex &&
              ctx.config.bridgedTokenPolicy.toHex == bridgedToken.policyId.toHex,
          "Local scripts do not match the live Config"
        )
        require(
          snapshot.trie.rootHash == ctx.state.spiRoot,
          "Swept trie does not match the live SPI root"
        )
        val pir = provider
            .findUtxo(pirInput)
            .await(timeout)
            .fold(e => throw IllegalStateException(e.toString), identity)
        require(pir.output.address == pegIn.address(network), "PIR not found at peg-in address")
        val datum = pir.output.inlineDatum
            .getOrElse(throw IllegalArgumentException("PIR has no inline PegInDatum"))
            .to[PegInDatum]
        val matches = provider
            .findUtxos(completedPegIns.address(network))
            .await(timeout)
            .fold(e => throw IllegalStateException(e.toString), identity)
            .toSeq
            .collect {
                case (i, o) if o.value.asset(completedPegIns.policyId, cpiAsset) == 1 => Utxo(i, o)
            }
        require(matches.size == 1, "Expected one completed-peg-ins singleton")
        val cpi = matches.head
        val root = cpi.output.inlineDatum
            .getOrElse(throw IllegalArgumentException("Completed-peg-ins UTxO has no datum"))
            .to[CompletedPegInsMerkleTreeDatum]
            .root
        val sweep = SweptPegInsProofService
            .proveFrom(snapshot.trie, datum.pegInUtxoId)
            .fold(e => throw IllegalArgumentException(e.message), identity)
        val tree = (if priorPegins.nonEmpty then
                        PegInCompleteTx.replayCompletedPegIns(priorPegins, snapshot.trie)
                    else
                        CompletedPegInsHistory
                            .reconstruct(
                              history,
                              pegIn.address(network).encode.get,
                              completedPegIns.address(network).encode.get,
                              pegIn.policyId.toHex,
                              completedPegIns.policyId.toHex,
                              cpiAsset.bytes.toHex,
                              root,
                              snapshot.trie
                            )
                            .map(_.tree)
        )
            .fold(e => throw IllegalArgumentException(e), identity)
        require(
          tree.rootHash == root,
          s"Reconstructed completed-peg-ins root ${tree.rootHash.toHex} != on-chain ${root.toHex}. Supply every earlier --prior-pegin."
        )
        val update = PegInCompleteTx
            .completedPegInsUpdate(tree, snapshot.trie, datum.pegInUtxoId)
            .fold(e => throw IllegalArgumentException(e), identity)
        val recipientData = LedgerToPlutusTranslation.getAddress(recipient).toData
        val digest = BifrostMessages.completionDigest(datum.pegInUtxoId, recipientData)
        Prepared(
          PegInCompleteTx.Inputs(pir, cpi, ctx.configUtxo, ctx.singletonUtxo),
          datum,
          recipient,
          recipientData,
          sweep,
          update,
          digest,
          BifrostMessages.completionSignText(digest)
        )
    }.toEither.left.map(_.getMessage)

    def build(
        provider: BlockchainProvider,
        sponsor: HdAccount,
        prepared: Prepared,
        references: References,
        signature: ByteString,
        excludeInputs: Set[TransactionInput] = Set.empty
    )(using ExecutionContext): Future[Transaction] = {
        def ref(script: Script.PlutusV3): Option[Utxo] =
            references.utxos.get(script.scriptHash).map { u =>
                require(u.output.scriptRef.contains(ScriptRef(script)), "Wrong reference script")
                u
            }
        PegInCompleteTx.build(
          provider,
          sponsor,
          PegInCompleteTx.Scripts(pegIn.script, completedPegIns.script, bridgedToken.script),
          PegInCompleteTx
              .ScriptRefs(ref(pegIn.script), ref(completedPegIns.script), ref(bridgedToken.script)),
          prepared.inputs,
          prepared.datum,
          prepared.recipient,
          prepared.recipientData,
          signature,
          prepared.update.insertProof,
          prepared.update.newRoot,
          prepared.sweep.sweepingTmInput0,
          prepared.sweep.proof,
          bridgedToken.policyId,
          tokenAsset,
          completedPegIns.policyId,
          cpiAsset,
          excludeInputs = references.excluded ++ excludeInputs
        )
    }
}

object PegInCompletion {
    final case class Prepared(
        inputs: PegInCompleteTx.Inputs,
        datum: PegInDatum,
        recipient: Address,
        recipientData: Data,
        sweep: SweptPegInsProofService.SpiMembershipProof,
        update: PegInCompleteTx.CompletedPegInsUpdate,
        digest: ByteString,
        signText: String
    )
    final case class References(utxos: Map[ScriptHash, Utxo], excluded: Set[TransactionInput])

    /** Fetch the selected reference for each hash, but exclude every discovered outpoint from
      * funding, including duplicate references left by earlier deployments.
      */
    def references(
        provider: BlockchainProvider,
        pairs: Seq[(ScriptHash, TransactionInput)],
        timeout: Duration
    )(using ExecutionContext): References = {
        val utxos = pairs.toMap.map { (hash, input) =>
            val utxo = provider
                .findUtxo(input)
                .await(timeout)
                .fold(e => throw IllegalStateException(e.toString), identity)
            require(
              utxo.output.scriptRef.exists(_.script.scriptHash == hash),
              "Wrong reference script"
            )
            hash -> utxo
        }
        References(utxos, pairs.map(_._2).toSet)
    }
}
