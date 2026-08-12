package binocular.cli.commands

import binocular.*
import binocular.bitcoin.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.{Command, CommandHelpers, Console}

import scalus.cardano.ledger.Utxo
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.uplc.builtin.ByteString
import scalus.utils.Hex.hexToBytes

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.boundary
import boundary.break
import scalus.utils.await
import cats.syntax.either.*

/** Watchtower Phase C: build + submit the `PegInRequest` mint tx for one BTC peg-in.
  *
  * Permissionless – only the watchtower's own wallet (fees + one-shot `input_ref`) is needed; no
  * depositor input. The BTC tx must already be in a block that the oracle's `confirmed_blocks_root`
  * commits to.
  */
case class PegInRequestCommand(
    btcTxId: String,
    dryRun: Boolean = false
) extends Command {

    /** How long the mint transaction stays submittable. It is also the [CLR-7] `created` anchor, so
      * a longer window only delays the Close grace period's start; one hour keeps the tx usable
      * across a slow submit without moving `created` far from the real creation time.
      */
    private val pegInRequestTtlSeconds: Long = 3600L

    override def execute(config: BinocularConfig): Int = boundary {
        Console.header("Binocular Peg-In Request")
        if dryRun then Console.warn("Dry-run mode — will not submit")
        println()

        given ec: ExecutionContext = ExecutionContext.global
        val timeout = config.oracle.transactionTimeout.seconds

        def hexBytes(label: String, s: String, expectedChars: Option[Int]): ByteString = {
            val isHex = s.length % 2 == 0 && s.forall(c => "0123456789abcdefABCDEF".contains(c))
            if !isHex || expectedChars.exists(_ != s.length) then {
                val want = expectedChars.fold("even-length hex")(n => s"$n hex chars")
                Console.error(s"Invalid $label: expected $want, got '$s'")
                break(1)
            }
            ByteString.fromHex(s)
        }

        // Fail fast on a malformed txid before any oracle/RPC work.
        hexBytes("BTC txid", btcTxId, Some(64))

        val setup = CommandHelpers.setupOracle(config).valueOr { err =>
            Console.error(err); break(1)
        }
        val provider = setup.provider
        val sponsorAddress = setup.sponsorAddress
        val oraclePolicyId = setup.script.scriptHash

        val (blueprint, blueprintSource) =
            try BifrostBlueprint.resolve(config.bridge.plutusJson)
            catch {
                case e: Exception =>
                    Console.error(s"Loading bridge blueprint: ${e.getMessage}"); break(1)
            }
        Console.info("blueprint", blueprintSource)
        // [CFG-7]: the config NFT asset name is the "BIFCFG" constant, no longer a parameter.
        val pegIn = PegInContract(
          blueprint,
          ByteString.fromArray(oraclePolicyId.bytes),
          hexBytes("bridge.config-nft-policy-id", config.bridge.configNftPolicyId, Some(56))
        )
        Console.info("Oracle policy", oraclePolicyId.toHex)
        Console.info("Peg-in policy", pegIn.policyId.toHex)
        Console.info("Peg-in address", pegIn.address(setup.network).encode.get)
        println()

        Console.step(1, "Reading oracle state")
        val oracleUtxo =
            try CommandHelpers.findOracleUtxo(provider, oraclePolicyId).await(timeout)
            catch { case e: Exception => Console.error(e.getMessage); break(1) }
        val chainState = CommandHelpers
            .parseChainState(oracleUtxo)
            .getOrElse { Console.error("Oracle UTxO has no valid ChainState datum"); break(1) }
        val rpc = new SimpleBitcoinRpc(config.bitcoinNode)
        val mpf = CommandHelpers
            .reconstructMpf(rpc, chainState, config.oracle.startHeight)
            .valueOr { err =>
                Console.error(s"Rebuilding confirmed-blocks MPF: $err"); break(1)
            }
        Console.success(s"Oracle at height ${chainState.ctx.height}, confirmed-blocks MPF rebuilt")
        println()

        Console.step(2, s"Building proof bundle for $btcTxId")
        val bundle = PegInProofBundle
            .produce(rpc, mpf, btcTxId)
            .await(timeout) match {
            case Right(b) => b
            case Left(err) =>
                Console.error(s"Proof bundle: $err"); break(1)
        }
        Console.info("Peg-in vout", bundle.pegInVout)
        Console.info("Peg-in amount (sat)", bundle.pegInAmountSat)
        Console.info("Depositor xonly", bundle.userSourceChainPubKey.toHex)
        println()

        // BTC outpoint: 32-byte internal (LE) txid ++ 4-byte little-endian vout.
        val vout = bundle.pegInVout
        val voutLE = Array[Byte](
          (vout & 0xff).toByte,
          ((vout >> 8) & 0xff).toByte,
          ((vout >> 16) & 0xff).toByte,
          ((vout >> 24) & 0xff).toByte
        )
        val pegInUtxoId =
            ByteString.fromArray(btcTxId.hexToBytes.reverse) ++ ByteString.fromArray(voutLE)

        // owner_auth does not gate completion: peg_in.ak authorizes the depositor with an embedded
        // BIP-322 signature keyed by user_source_chain_pub_key, bound to the deposit at mint. It
        // DOES gate the Close path ([CLR-9]), so an inert, never-satisfiable signature credential
        // means only the completion can ever retire this request.
        //
        // [CLR-7]: `created` must equal the mint transaction's validity upper bound, which must be
        // finite. Derive both from one slot so the datum and the tx cannot disagree: the ledger
        // turns the slot back into POSIX ms with the same slot config the builder used.
        val slotConfig = provider.cardanoInfo.slotConfig
        val validToSlot =
            slotConfig.instantToSlot(java.time.Instant.now().plusSeconds(pegInRequestTtlSeconds))
        val created = BigInt(slotConfig.slotToTime(validToSlot))
        val datum = PegInDatum(
          ownerAuth = AuthorizationMethod.CardanoSignature(ByteString.empty),
          sourceChainPegInRawTx = bundle.rawTxHex,
          sourceChainPegInRawTxIndex = BigInt(bundle.txIndex),
          pegInUtxoId = pegInUtxoId,
          pegInAmount = BigInt(bundle.pegInAmountSat),
          userSourceChainPubKey = bundle.userSourceChainPubKey,
          created = created
        )
        val request = PegInRequest(
          expectedDatum = datum,
          blockHeader = bundle.blockHeader,
          blockHeaderInSourceChainInclusionProof = bundle.mpfHeaderInclusionProof,
          txInBlockHeaderInclusionProof = ScalusList(bundle.txInBlockMerklePath*)
        )

        Console.step(3, "Selecting one-shot input_ref")
        val walletUtxos = provider.findUtxos(sponsorAddress).await(timeout) match {
            case Right(utxos) => utxos.toList.map { case (i, o) => Utxo(i, o) }
            case Left(err)    => Console.error(s"Fetching wallet UTxOs: $err"); break(1)
        }
        val inputRefUtxo = walletUtxos
            .filter(u => u.output.value.assets.isEmpty && u.output.value.coin.value >= 10_000_000L)
            .sortBy(-_.output.value.coin.value)
            .headOption
            .getOrElse {
                Console.error("No pure-ADA wallet UTxO (>=10 ADA) for input_ref"); break(1)
            }
        Console.info(
          "input_ref",
          s"${inputRefUtxo.input.transactionId.toHex}#${inputRefUtxo.input.index}"
        )
        println()

        if dryRun then {
            Console.success("Dry-run complete (assembled datum + request; not building tx)")
            break(0)
        }

        Console.step(4, "Building + submitting peg-in mint tx")
        val tx =
            try
                PegInRequestTx
                    .build(
                      provider,
                      setup.hdAccount,
                      pegIn,
                      oracleUtxo,
                      inputRefUtxo,
                      request,
                      validToSlot
                    )
                    .await(timeout)
            catch {
                case e: Exception =>
                    Console.error(s"Building tx: ${e.getMessage}")
                    Option(e.getCause).foreach(c => Console.error(s"Cause: ${c.getMessage}"))
                    break(1)
            }
        val txHash = OracleTransactions.submitTx(provider, tx, timeout) match {
            case Right(h)  => h
            case Left(err) => Console.error(s"Submit: $err"); break(1)
        }

        println()
        Console.separator()
        Console.tx("Peg-in request TX", txHash)
        Console.info("Peg-in address", pegIn.address(setup.network).encode.get)
        Console.info("Peg-in policy", pegIn.policyId.toHex)
        Console.separator()
        0
    }
}
