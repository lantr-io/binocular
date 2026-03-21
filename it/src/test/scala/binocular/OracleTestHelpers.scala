package binocular

import binocular.cli.CommandHelpers
import scalus.cardano.address.Address
import scalus.cardano.ledger.{ScriptHash, Utxo}
import scalus.cardano.node.BlockchainProvider
import scalus.utils.await

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

/** Shared helpers for Binocular integration tests. */
trait OracleTestHelpers {

    protected def getUtxos(provider: BlockchainProvider, address: Address): List[Utxo] = {
        given ExecutionContext = provider.executionContext
        val result = provider.findUtxos(address).await(30.seconds)
        result match {
            case Right(u) => u.map { case (input, output) => Utxo(input, output) }.toList
            case Left(_)  => List.empty
        }
    }

    protected def findOracleUtxo(
        provider: BlockchainProvider,
        nftPolicyId: ScriptHash
    ): Utxo = {
        given ExecutionContext = provider.executionContext
        CommandHelpers.findOracleUtxo(provider, nftPolicyId).await(30.seconds)
    }
}
