package binocular

import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, ScriptHash, Utxo}
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
        scriptAddress: Address,
        nftPolicyId: ScriptHash
    ): Utxo = {
        val utxos = getUtxos(provider, scriptAddress)
        utxos
            .find(u => u.output.value.hasAsset(nftPolicyId, AssetName.empty))
            .getOrElse {
                throw new RuntimeException(
                  s"No oracle UTxO with NFT $nftPolicyId found at $scriptAddress"
                )
            }
    }
}
