package binocular.cli

import binocular.bitcoin.BitcoinRpc
import binocular.oracle.{BitcoinValidatorParams, BlockHeader, ChainState}
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF

/** The headers to submit this cycle, where they attach, and a human-readable
  * status/idle line for the daemon to print.
  */
case class UpdatePlan(
    headers: ScalusList[BlockHeader],
    parentPath: ScalusList[BigInt],
    logContext: String
)

/** Strategy that decides what the oracle daemon submits each cycle.
  *
  * `HonestPlanner` fetches the real Bitcoin chain; `RoguePlanner` (Eve) mines
  * rogue blocks. The daemon owns everything else (submit/poll/confirm/adopt).
  */
trait UpdatePlanner {
    def planUpdate(
        rpc: BitcoinRpc,
        chainState: ChainState,
        mpf: OffChainMPF,
        validityTime: BigInt,
        params: BitcoinValidatorParams
    ): UpdatePlan
}
