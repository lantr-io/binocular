package binocular.cli.commands

import binocular.BinocularConfig
import binocular.bitcoin.SimpleBitcoinRpc
import binocular.cli.{Command, CommandHelpers, Console}
import binocular.watchtower.{BridgeState, SweptPegInsProofService, TreasuryMovementValidator}

import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, Credential, ScriptHash, Utxo}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.{boundary, Try}
import boundary.break
import scalus.utils.await

/** Serve the [CPI-9] swept-peg-ins membership proof for one deposit outpoint ([SPI-4]).
  *
  * Thin transport over [[SweptPegInsProofService]], which owns the [SPI-6] walk and reconciliation.
  * Prints a JSON object whose fields are exactly what the [CPI-9] redeemer needs: the proven value
  * (`sweeping_tm_input_0`) and the membership proof as Plutus `Data` CBOR.
  *
  * Serving is trustless ([SPI-4]): the proof is verified on-chain against the singleton's attested
  * `spi_root`, so a wrong one simply fails `mpf.has`. Anyone may run this command.
  */
case class SpiProofCommand(
    outpoint: String
) extends Command {

    override def execute(config: BinocularConfig): Int = boundary {
        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        val pegInUtxoId = CommandHelpers.parseBtcOutpoint(outpoint) match {
            case Right(b)  => b
            case Left(err) => Console.error(s"Invalid outpoint: $err"); break(1)
        }

        val provider = config.cardano.createBlockchainProvider() match {
            case Right(p)  => p
            case Left(err) => Console.error(s"Creating blockchain provider: $err"); break(1)
        }
        val network = config.cardano.scalusNetwork

        // 1. The Config UTxO: field 3 is `bridge_state_policy`, the singleton's NFT policy.
        val configNftPolicy = ScriptHash.fromHex(config.bridge.configNftPolicyId)
        val configNftAsset = AssetName(ByteString.fromHex(config.bridge.configNftAssetName))
        val configAddress = Address(network, Credential.ScriptHash(configNftPolicy))
        val configUtxo = provider
            .findUtxos(configAddress)
            .await(timeout)
            .toOption
            .flatMap(_.collectFirst {
                case (in, out) if out.value.hasAsset(configNftPolicy, configNftAsset) =>
                    Utxo(in, out)
            })
            .getOrElse {
                Console.error(s"no UTxO carrying the config NFT at $configAddress")
                break(1)
            }
        val bridgeStatePolicy = configUtxo.output.inlineDatum match {
            case Some(Data.Constr(0, fields)) =>
                fields.asScala.toList.lift(3) match {
                    case Some(Data.B(p)) => p
                    case other => Console.error(s"config field 3 is not bytes: $other"); break(1)
                }
            case other =>
                Console.error(s"config datum is not a Constr 0 inline datum: $other"); break(1)
        }
        Console.info("bridge_state_policy", bridgeStatePolicy.toHex)

        // 2. The bridge state singleton, authenticated by the NFT (bridge_state_policy, "BSS")
        //    and decoded as BridgeState BY NAME ([LIB-1]). No fallback: if it does not exist or
        //    does not decode, say so plainly rather than inventing a root.
        val singletonPolicy = ScriptHash.fromHex(bridgeStatePolicy.toHex)
        val singletonAddress = Address(network, Credential.ScriptHash(singletonPolicy))
        val bssAsset = AssetName(TreasuryMovementValidator.BridgeStateAssetName)
        val singletonUtxo = provider
            .findUtxos(singletonAddress)
            .await(timeout)
            .toOption
            .flatMap(_.collectFirst {
                case (in, out) if out.value.hasAsset(singletonPolicy, bssAsset) => Utxo(in, out)
            })
            .getOrElse {
                // TODO(bridge-state migration): binocular's TM Confirm still runs the CPO-trie
                // flow, and deploy still writes the TRIE policy into config field 3 — so on a
                // pre-migration deployment there is no "BSS" singleton to read, only a "CPO" trie
                // UTxO. This command implements the singleton shape the rev-5.4 spec defines and
                // refuses to serve until the migration lands.
                Console.error(
                  s"no UTxO carrying the (bridge_state_policy, \"BSS\") NFT at $singletonAddress. " +
                      "If this deployment predates the bridge-state migration, config field 3 " +
                      "still publishes the completed-peg-outs trie policy and no singleton " +
                      "exists yet — there is no spi_root to prove against."
                )
                break(1)
            }
        val state = singletonUtxo.output.inlineDatum
            .flatMap(d => Try(d.to[BridgeState]).toOption)
            .getOrElse {
                Console.error(
                  "the singleton UTxO's datum does not decode as the 4-field BridgeState — " +
                      "refusing to guess at a root ([LIB-1])"
                )
                break(1)
            }
        Console.info("spi_root", state.spiRoot.toHex)
        Console.info("head", state.treasuryUtxoId.toHex)

        // 3. Walk the Bitcoin treasury chain from the confirmed head and serve ([SPI-6]).
        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        val fetch = SweptPegInsProofService.rpcFetcher(rpc, timeout)
        SweptPegInsProofService.serve(state, fetch, pegInUtxoId) match {
            case Left(err) =>
                Console.error(err.message)
                1
            case Right(proof) =>
                val json = ujson.Obj(
                  "peg_in_utxo_id" -> proof.pegInUtxoId.toHex,
                  "sweeping_tm_input_0" -> proof.sweepingTmInput0.toHex,
                  "spi_root" -> proof.spiRoot.toHex,
                  // The [CPI-9] `peg_in_swept_membership_proof`, as Plutus Data CBOR.
                  "proof_cbor" -> ByteString.fromArray(proof.proof.toData.toCbor).toHex
                )
                println(ujson.write(json, indent = 2))
                0
        }
    }
}
