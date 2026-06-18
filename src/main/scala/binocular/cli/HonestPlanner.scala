package binocular.cli

import binocular.bitcoin.BitcoinRpc
import binocular.oracle.*
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scalus.utils.await

/** Today's honest behavior: fetch the real Bitcoin chain, with reorg detection and the 150-block
  * fork-tree cap. Extracted from the original RunCommand loop.
  *
  * @param batchSize
  *   config.oracle.maxHeadersPerTx
  */
class HonestPlanner(batchSize: Int)(using ExecutionContext) extends UpdatePlanner {

    private val maxForkTreeBestChain = 150

    override def planUpdate(
        rpc: BitcoinRpc,
        chainState: ChainState,
        mpf: OffChainMPF,
        validityTime: BigInt,
        params: BitcoinValidatorParams
    ): UpdatePlan = {
        val bitcoinInfo = rpc.getBlockchainInfo().await(30.seconds)
        val bitcoinTip = bitcoinInfo.blocks.toLong

        val (parentPath, reorgStartHeight) =
            CommandHelpers.detectReorgAndComputePath(rpc, chainState, mpf)

        val highestKnown =
            if chainState.forkTree.nonEmpty then
                chainState.forkTree.highestHeight(chainState.ctx.height).toLong
            else chainState.ctx.height.toLong

        val bestChainBlocks =
            if chainState.forkTree.nonEmpty then (highestKnown - chainState.ctx.height.toLong).toInt
            else 0
        val maxNewBlocks = maxForkTreeBestChain - bestChainBlocks
        val effectiveStart = reorgStartHeight

        val headers: scala.List[BlockHeader] =
            if bitcoinTip >= effectiveStart && maxNewBlocks > 0 then {
                val startHeight = effectiveStart
                val endHeight = Math.min(
                  Math.min(bitcoinTip, startHeight + batchSize - 1),
                  startHeight + maxNewBlocks - 1
                )

                def fetchHeaders(
                    heights: scala.List[Long],
                    acc: scala.List[BlockHeader]
                ): Future[scala.List[BlockHeader]] =
                    heights match {
                        case Nil => Future.successful(acc.reverse)
                        case h :: tail =>
                            for {
                                hashHex <- rpc.getBlockHash(h.toInt)
                                headerInfo <- rpc.getBlockHeader(hashHex)
                                header = BitcoinChainState.convertHeader(headerInfo)
                                rest <- fetchHeaders(tail, header :: acc)
                            } yield rest
                    }
                Console.log(
                  s"Fetching blocks $startHeight..$endHeight (${endHeight - startHeight + 1} headers)"
                )
                fetchHeaders((startHeight to endHeight).toList, scala.Nil).await(60.seconds)
            } else scala.Nil

        val behind = bitcoinTip - highestKnown
        val status = if behind == 0 then "up to date" else s"behind: $behind"
        UpdatePlan(
          ScalusList.from(headers),
          parentPath,
          s"Polling... tip: $bitcoinTip | oracle: $highestKnown | $status"
        )
    }
}
