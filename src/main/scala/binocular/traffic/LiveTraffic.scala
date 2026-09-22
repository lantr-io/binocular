package binocular.traffic

import binocular.BinocularConfig
import binocular.bitcoin.*
import binocular.cli.{CommandHelpers, Console, OracleSetup}
import binocular.cli.commands.BridgeSweepSetup
import binocular.notify.Notifier
import binocular.server.{ProofApi, ProofService}
import binocular.watchtower.*
import org.bitcoins.core.protocol.transaction.{Transaction as BtcTransaction, TransactionOutPoint}
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.node.{BlockfrostProvider, TransactionStatus}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.uplc.builtin.{ByteString, FromData}
import scodec.bits.ByteVector
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.{Random, Try}
import scala.util.control.NonFatal

/** The stateless reconciler ([TRF-106]): every tick reads both ledgers, asks [[TrafficPlan]] for at
  * most one action, and performs it. Existing builders and proof services own the protocol; this
  * class only connects them. Nothing is remembered between ticks except a cache of raw Bitcoin
  * transactions and the last summary line.
  */
final class LiveTraffic(config: BinocularConfig, notifier: Notifier)(using ExecutionContext) {
    import LiveTraffic.checked
    checked(config.traffic.validate(config))
    private val settings = config.traffic
    private val timeout = config.oracle.transactionTimeout.max(180).seconds
    private val setup: OracleSetup = checked(CommandHelpers.setupOracle(config))
    private val provider = setup.provider match {
        case p: BlockfrostProvider => p
        case _ =>
            throw IllegalArgumentException("Traffic requires a Blockfrost-compatible provider")
    }
    private val rpc = new SimpleBitcoinRpc(config.bitcoinNode, java.time.Duration.ofSeconds(180))
    private val wallet = checked(
      Bip86Wallet.fromMnemonic(config.wallet.mnemonic, config.bitcoinNode.bitcoinNetwork)
    )
    private val qAuth = wallet.identity.outputKey
    private val fundingScript = ByteString.fromArray(wallet.funding.scriptPubKey.toArray)
    private val blueprint = BifrostBlueprint.resolve(config.bridge.plutusJson)._1
    private val configPolicy = ScriptHash.fromHex(config.bridge.configNftPolicyId)
    private val configAsset = AssetName(ByteString.fromHex(config.bridge.configNftAssetName))
    private val configBytes = ByteString.fromArray(configPolicy.bytes)
    private val pegIn =
        PegInContract(blueprint, ByteString.fromArray(setup.script.scriptHash.bytes), configBytes)
    private val pegOut = PegOutContract(blueprint, configBytes)
    private val token = BridgedTokenContract(blueprint, configBytes)
    private val cpiInput = input(config.bridge.completedPegInsOneShotRef)
    private val cpi = CompletedPegInsContract(
      blueprint,
      configBytes,
      TxOutRef(TxId(cpiInput.transactionId), cpiInput.index)
    )
    private val tokenAsset = AssetName(ConfigDatum.BridgedTokenAssetName)
    private val completion = PegInCompletion(pegIn, cpi, token, tokenAsset)
    private val history = checked(ProviderChainHistory.from(provider, timeout))
    private val proofs = new ProofService(
      provider,
      history,
      rpc,
      setup.network,
      setup.script.scriptHash,
      configPolicy,
      configAsset,
      config.oracle.startHeight,
      timeout
    )
    private val sponsor = setup.sponsorAddress
    private val walk = new DepositWalk(rpc, qAuth, timeout)
    private val random = new Random
    private var lastSummary = ""

    private def await[A](future: Future[A]): A = Await.result(future, timeout)
    private def input(ref: String): TransactionInput = {
        val parts = ref.split("#")
        require(parts.length == 2 && parts(1).toInt >= 0, "Expected Cardano TXID#INDEX")
        TransactionInput(TransactionHash.fromHex(parts(0)), parts(1).toInt)
    }
    private def address(hash: ByteString): Address =
        Address(setup.network, Credential.ScriptHash(ScriptHash.fromHex(hash.toHex)))
    private def utxos(addr: Address): Utxos = checked(await(provider.findUtxos(addr)))
    private def tip: Long = await(provider.fetchLatestBlock).slot
        .getOrElse(throw IllegalStateException("Cardano tip has no slot"))
    private def checkScriptIdentity(
        ctx: BridgeSweepSetup.SingletonContext
    ): BridgeSweepSetup.SingletonContext = {
        require(
          ctx.config.pegInScriptHash.toHex == pegIn.policyId.toHex &&
              ctx.config.pegOutScriptHash.toHex == pegOut.policyId.toHex &&
              ctx.config.completedPegInsPolicy.toHex == cpi.policyId.toHex &&
              ctx.config.bridgedTokenPolicy.toHex == token.policyId.toHex,
          "Local scripts do not match the live Config"
        )
        ctx
    }
    private def context() =
        checkScriptIdentity(
          checked(
            BridgeSweepSetup.loadSingletonContext(
              provider,
              Address(setup.network, Credential.ScriptHash(configPolicy)),
              configPolicy,
              AssetName(ConfigDatum.ConfigNftAssetName),
              setup.network,
              timeout
            )
          )
        )
    private def schedule(ctx: BridgeSweepSetup.SingletonContext) =
        checked(TrafficSchedule.at(tip, settings.virtualEpochSlots, ctx.config.params.schedule))
    private def treeParams(ctx: BridgeSweepSetup.SingletonContext, y51: ByteString) =
        PeginTreeParams(
          ByteVector(y51.bytes),
          ByteVector(ctx.config.yFederation.bytes),
          ctx.config.params.federationCsvBlocks.toInt,
          ctx.config.params.peginRefundTimeoutBlocks.toInt
        )
    private def treasuryAddress(ctx: BridgeSweepSetup.SingletonContext) =
        address(ctx.config.treasuryInfoPolicyId)
    private def treasuryKey(ctx: BridgeSweepSetup.SingletonContext): ByteString =
        singleton(
          treasuryAddress(ctx),
          ScriptHash.fromHex(ctx.config.treasuryInfoPolicyId.toHex),
          AssetName(ConfigDatum.TreasuryInfoAssetName)
        ).output.inlineDatum.get.to[TreasuryInfoDatum].currentSposFrostKey
    private def singleton(addr: Address, policy: ScriptHash, asset: AssetName): Utxo = {
        val matches = utxos(addr).toSeq.collect {
            case (i, o) if o.value.asset(policy, asset) == 1 => Utxo(i, o)
        }
        require(matches.size == 1, "Expected one bridge singleton")
        matches.head
    }
    private def datum[A: FromData](output: TransactionOutput): Option[A] =
        output.inlineDatum.flatMap(d => Try(d.to[A]).toOption)
    private def bitcoinTxid(outpoint: TransactionOutPoint): String = outpoint.txIdBE.hex
    private def proved(id: ByteString): Option[ProofService.DepositProof] =
        proofs.depositProofFor(id) match {
            case Right(proof)                                            => Some(proof)
            case Left(error) if LiveTraffic.retryableDepositProof(error) => None
            case Left(error) =>
                Console.logWarn(s"Traffic: deposit proof ${error.code}: ${error.message}")
                None
        }

    /** Submit and wait for inclusion, so that the next tick observes the result instead of building
      * the same transaction again.
      */
    private def submit(tx: Transaction, label: String): Unit = {
        val hash = checked(await(provider.submit(tx)))
        require(hash == tx.id, "Cardano submission returned another transaction ID")
        Console.log(s"Traffic: $label ${hash.toHex}")
        val deadline = System.nanoTime + 15.minutes.toNanos
        while await(provider.checkTransaction(hash)) != TransactionStatus.Confirmed do {
            require(System.nanoTime < deadline, s"$label ${hash.toHex} unconfirmed after 15 min")
            Thread.sleep(10000)
        }
        require(
          await(provider.fetchTransactionInfo(hash.toHex)).validContract,
          "Cardano script failed"
        )
    }
    private def references(): PegInCompletion.References =
        PegInCompletion.references(
          provider,
          CommandHelpers.refScriptPairs(
            config,
            CommandHelpers.refScriptScanAddresses(config, setup.network, sponsor)
          ),
          timeout
        )

    /** Read-only startup check, also used by watchtower --dry-run. */
    def check(): Unit = {
        val expected = config.bitcoinNode.bitcoinNetwork match {
            case BitcoinNetwork.Testnet  => "test"
            case BitcoinNetwork.Testnet4 => "testnet4"
            case BitcoinNetwork.Regtest  => "regtest"
            case BitcoinNetwork.Mainnet => throw IllegalArgumentException("Traffic refuses mainnet")
        }
        require(
          await(rpc.getBlockchainInfo()).chain == expected,
          "Bitcoin node network differs from configuration"
        )
        val ctx = context()
        val validSchedule = settings.validateSchedule(ctx.config.params.schedule)
        validSchedule.left.foreach(Console.logError)
        checked(validSchedule)
        val params = ctx.config.params
        require(
          settings.maxAmountSat >= params.minPegOutFbtc,
          s"max-amount-sat is below the live min_peg_out_fbtc ${params.minPegOutFbtc}" // [TRF-121]
        )
        require(
          params.perPegoutFee >= 0 && params.minPegOutFbtc - params.perPegoutFee >= 330,
          "The live peg-out minimum leaves dust after the fee"
        )
        require(
          params.federationCsvBlocks.isValidInt && params.peginRefundTimeoutBlocks.isValidInt,
          "Invalid bridge CSV delays"
        )
        tip
        ()
    }

    /** One tick of [TRF-110]: observe, decide, act. Errors propagate to the daemon loop. A dry run
      * logs what it would do and does nothing.
      */
    def tick(dryRun: Boolean = false): Unit = {
        val observed = observe()
        val summary = TrafficPlan.summary(observed.obs)
        if dryRun || summary != lastSummary then { // [TRF-48]
            Console.log(s"Traffic: $summary")
            lastSummary = summary
        }
        val draws = Draws(random.nextDouble(), random.nextDouble(), random.nextDouble())
        val action = TrafficPlan.decide(
          observed.obs,
          settings.maxAmountSat,
          settings.meanInterval.toSeconds,
          draws
        )
        action.foreach { a =>
            Console.log(s"Traffic: ${if dryRun then "would " else ""}${LiveTraffic.describe(a)}")
            if !dryRun then
                a match {
                    case Action.Refund(d)       => refund(observed.ctx, d)
                    case Action.Complete(_, r)  => complete(observed.swept, r)
                    case Action.Request(d)      => request(d)
                    case Action.Deposit(amount) => deposit(observed.ctx, observed.coins, amount)
                    case Action.PegOut(amount)  => withdraw(observed.ctx, amount)
                }
        }
    }

    private final case class Observed(
        ctx: BridgeSweepSetup.SingletonContext,
        coins: Seq[ScannedBitcoinOutput],
        swept: ProofService.SweptSnapshot,
        obs: Observation
    )

    private def observe(): Observed = {
        val ctx = context()
        val params = ctx.config.params
        val scan = await(rpc.scanAddress(wallet.funding.address)) // once per tick, [TRF-108]
        val coins = await(LiveTraffic.spendable(rpc, scan))
        val swept = checked(proofs.sweptSnapshot())
        checkScriptIdentity(swept.context)
        val requests = utxos(pegIn.address(setup.network)).flatMap { (input, out) =>
            datum[PegInDatum](out)
                .filter(_.userSourceChainPubKey == ByteString.fromArray(qAuth.toArray))
                .map(_.pegInUtxoId -> input)
        }
        val deposits = walk.deposits(scan.outputs.map(_.outpoint)).map { d =>
            val id = LiveTraffic.pegInUtxoId(d.outpoint)
            val txid = bitcoinTxid(d.outpoint)
            val isSwept = swept.trie.get(id).nonEmpty
            val requested = requests.get(id)
            val unspent = !isSwept &&
                await(rpc.isTxOutUnspent(txid, PegInDeposit.DepositVout, includeMempool = true))
            val confirmations =
                if unspent then await(rpc.getRawTransaction(txid)).confirmations else 0
            val inOracle = requested.isEmpty && unspent && proved(id).isDefined
            DepositView(
              d.outpoint,
              d.amountSat,
              confirmations,
              unspent,
              isSwept,
              requested,
              inOracle
            )
        }
        val held = utxos(sponsor).values.map(_.value.asset(token.policyId, tokenAsset)).sum
        val open = utxos(pegOut.address(setup.network)).values.count(out =>
            datum[PegOutDatum](out).exists(_.sourceChainDestinationAddress == fundingScript)
        )
        val window = schedule(ctx)
        // After the scheduled key rotation, leaving six hours for Bitcoin/oracle inclusion.
        val inWindow = window.tipSlot >= window.updateYDeadline &&
            window.finalCutoff - window.tipSlot >= 21600 && window.nextBatch.nonEmpty
        Observed(
          ctx,
          coins,
          swept,
          Observation(
            deposits,
            coins.map(_.amountSat).sum,
            held,
            params.minPegOutFbtc.toLong,
            params.peginRefundTimeoutBlocks.toInt,
            inWindow,
            open
          )
        )
    }

    private def deposit(
        ctx: BridgeSweepSetup.SingletonContext,
        coins: Seq[ScannedBitcoinOutput],
        amountSat: Long
    ): Unit = {
        val tree = treeParams(ctx, treasuryKey(ctx))
        val tx = await(LiveTraffic.depositTransaction(rpc, wallet, tree, amountSat, coins))
        val id = await(rpc.sendRawTransaction(tx.hex))
        require(id == tx.txIdBE.hex, "Bitcoin submission returned another transaction ID")
        Console.log(s"Traffic: deposit $id of $amountSat sat")
    }

    private def request(d: DepositView): Unit = {
        val proof = proved(LiveTraffic.pegInUtxoId(d.outpoint))
            .getOrElse(throw IllegalStateException("Deposit is no longer provable in the oracle"))
        val bundle = proof.bundle
        val slotConfig = provider.cardanoInfo.slotConfig
        val ttl = slotConfig.instantToSlot(
          java.time.Instant
              .ofEpochMilli(slotConfig.slotToTime(tip))
              .plusSeconds(config.bridge.peginRequestTtlSeconds)
        )
        val datum = PegInDatum(
          AuthorizationMethod.CardanoSignature(ByteString.empty),
          bundle.rawTxHex,
          bundle.txIndex,
          bundle.pegInUtxoId,
          bundle.pegInAmountSat,
          bundle.userSourceChainPubKey,
          BigInt(slotConfig.slotToTime(ttl))
        )
        val excluded = references().excluded
        val funding = CardanoFunding.eligible(utxos(sponsor), sponsor, excluded)
        val oneShot = funding.toSeq
            .filter { (_, out) =>
                out.value.assets.isEmpty && out.value.coin.value >= 10000000
            }
            .sortBy(-_._2.value.coin.value)
            .headOption
            .getOrElse(
              throw IllegalStateException("Traffic needs a pure-ADA input of at least 10 ADA")
            )
        val tx = await(
          PegInRequestTx.build(
            provider,
            setup.hdAccount,
            pegIn,
            proof.oracle.utxo,
            Utxo(oneShot._1, oneShot._2),
            PegInRequest(
              datum,
              bundle.blockHeader,
              bundle.mpfHeaderInclusionProof,
              PList(bundle.txInBlockMerklePath*)
            ),
            ttl,
            excludeInputs = excluded
          )
        )
        submit(tx, "peg-in request")
    }

    private def complete(snapshot: ProofService.SweptSnapshot, request: TransactionInput): Unit = {
        val pir = checked(await(provider.findUtxo(request)))
        val prepared = checked(
          completion.prepare(provider, history, snapshot, pir.input, sponsor, timeout)
        )
        val signature = ByteString.fromArray(
          wallet.identity
              .signMessage(
                ByteVector(prepared.signText.getBytes(java.nio.charset.StandardCharsets.UTF_8))
              )
              .toArray
        )
        val tx = await(
          completion.build(provider, setup.hdAccount, prepared, references(), signature)
        )
        submit(tx, "peg-in completion")
    }

    private def withdraw(ctx: BridgeSweepSetup.SingletonContext, amountSat: Long): Unit = {
        val datum = PegOutDatum(
          AuthorizationMethod.CardanoSignature(
            ByteString.fromArray(setup.hdAccount.paymentKeyHash.bytes)
          ),
          fundingScript,
          ctx.config.params.perPegoutFee,
          BigInt(provider.cardanoInfo.slotConfig.slotToTime(tip))
        )
        val tx = PegOutRequestTx.build(
          provider.cardanoInfo,
          setup.hdAccount,
          utxos(sponsor),
          references().excluded,
          pegOut.address(setup.network),
          token.policyId,
          tokenAsset,
          amountSat,
          datum
        )
        submit(tx, "peg-out request")
    }

    /** [TRF-116], [TRF-80], [TRF-117], [TRF-82]. */
    private def refund(ctx: BridgeSweepSetup.SingletonContext, d: DepositView): Unit = {
        val outpoint = d.outpoint.toHumanReadableString
        val script = walk.script(d.outpoint)
        val policyHex = ctx.config.treasuryInfoPolicyId.toHex
        val assetHex = ConfigDatum.TreasuryInfoAssetName.toHex
        // Every Y_51 the treasury-info datum ever carried, plus the live one.
        val candidates = checked(history.addressHistory(treasuryAddress(ctx).encode.get))
            .filter(_.quantityOf(policyHex, assetHex) == 1)
            .flatMap(_.inlineDatum.flatMap(dat => Try(dat.to[TreasuryInfoDatum]).toOption))
            .map(_.currentSposFrostKey) :+ treasuryKey(ctx)
        val tree = candidates.distinct.iterator
            .map(y51 => Taproot.peginTree(treeParams(ctx, y51), qAuth))
            .collectFirst { case Right(t) if t.scriptPubKey.asmBytes == script => t }
        tree match {
            case None =>
                val message = s"No Y_51 in the treasury-info history rebuilds deposit $outpoint"
                Console.logError(s"Traffic: $message")
                notifier.error("traffic", message) // [TRF-49]
            case Some(t) =>
                val txid = bitcoinTxid(d.outpoint)
                require(
                  await(rpc.isTxOutUnspent(txid, PegInDeposit.DepositVout, includeMempool = true)),
                  s"deposit $outpoint was spent before the refund" // [TRF-80]
                )
                val rate = await(LiveTraffic.feeRateSatPerKvb(rpc))
                val tx = checked(
                  PegInRefund.build(
                    t,
                    d.outpoint,
                    d.amountSat,
                    ctx.config.params.peginRefundTimeoutBlocks.toInt,
                    wallet.funding.scriptPubKey,
                    wallet.identity,
                    rate
                  )
                )
                val fee = d.amountSat - tx.outputs.head.value.satoshis.toLong
                require(
                  fee <= TrafficPlan.FeeCapSat,
                  s"Refund fee $fee sat exceeds the cap of ${TrafficPlan.FeeCapSat} sat"
                )
                val id = await(rpc.sendRawTransaction(tx.hex))
                val message = s"Refund $id broadcast for deposit $outpoint (${d.amountSat} sat)"
                Console.log(s"Traffic: $message")
                notifier.success("traffic", message) // [TRF-82]
        }
    }
}

object LiveTraffic {

    /** [TRF-72] and [TRF-120]: `estimatesmartfee` target and mode, and the fallback rate. */
    val FeeConfirmTarget = 6
    val FeeEstimateMode = "economical"
    val FallbackFeeRateSatPerKvb = 1000L

    private[traffic] def retryableDepositProof(error: ProofApi.ApiError): Boolean =
        error.code == "tx_not_confirmed" || error.code == "oracle_lagging"

    /** The 36-byte `peg_in_utxo_id`: the txid in wire order, then the vout little-endian. */
    private[traffic] def pegInUtxoId(outpoint: TransactionOutPoint): ByteString =
        CpoTrieMirror.hintBytes(
          ByteString.fromArray(outpoint.txId.bytes.toArray),
          outpoint.vout.toLong
        )

    private[traffic] def describe(action: Action): String = {
        def at(d: DepositView) = d.outpoint.toHumanReadableString
        action match {
            case Action.Refund(d) => s"refund ${at(d)} (${d.amountSat} sat)"
            case Action.Complete(d, r) =>
                s"complete ${at(d)} via ${r.transactionId.toHex}#${r.index}"
            case Action.Request(d)   => s"request ${at(d)}"
            case Action.Deposit(sat) => s"deposit $sat sat"
            case Action.PegOut(sat)  => s"peg out $sat sat"
        }
    }

    private[traffic] def checked[E, A](result: Either[E, A]): A =
        result.fold(e => throw IllegalStateException(s"Traffic dependency failed: $e"), identity)

    /** What the funding address can spend now. The scan is the confirmed UTXO set: a coinbase
      * output needs 100 confirmations, and an output the mempool already spends (the last deposit's
      * inputs) must not be spent again.
      */
    private[traffic] def spendable(rpc: BitcoinRpc, scan: BitcoinUtxoScan)(using
        ExecutionContext
    ): Future[Seq[ScannedBitcoinOutput]] =
        Future
            .traverse(
              scan.outputs.filter(out => !out.coinbase || scan.height - out.height + 1 >= 100)
            ) { out =>
                rpc
                    .isTxOutUnspent(out.outpoint.txIdBE.hex, out.outpoint.vout.toInt, true)
                    .map(unspent => Option.when(unspent)(out))
            }
            .map(_.flatten)

    /** The sat/kvB rate for the next transaction: the node's estimate, else the fallback, and never
      * below the node's mempool floor. A node that cannot answer either question is not an error
      * here; the fallback exists for it.
      */
    private[traffic] def feeRateSatPerKvb(rpc: BitcoinRpc)(using ExecutionContext): Future[Long] = {
        val estimate = rpc
            .estimateSmartFeeSatPerKvb(FeeConfirmTarget, FeeEstimateMode)
            .map(_.filter(_ > 0))
            .recover { case NonFatal(_) => None }
        val floor = rpc
            .getMempoolMinFeeSatPerKvb()
            .map(rate => Option.when(rate >= 0)(rate))
            .recover { case NonFatal(_) => None }
        for
            estimated <- estimate
            minimum <- floor
        yield {
            val rate =
                math.max(estimated.getOrElse(FallbackFeeRateSatPerKvb), minimum.getOrElse(0L))
            Console.log(
              s"Traffic: fee rate $rate sat/kvB " +
                  s"(${if estimated.isDefined then "estimate" else "fallback"}, " +
                  s"mempool floor ${minimum.getOrElse("unavailable")})"
            )
            rate
        }
    }

    private[traffic] def depositTransaction(
        rpc: BitcoinRpc,
        wallet: Bip86Wallet,
        tree: PeginTreeParams,
        amountSat: Long,
        coins: Seq[ScannedBitcoinOutput]
    )(using ExecutionContext): Future[BtcTransaction] =
        feeRateSatPerKvb(rpc).map { rate =>
            val tx = checked(
              PegInDeposit.build(
                tree,
                wallet.identity.outputKey,
                amountSat,
                coins,
                wallet.funding,
                rate
              )
            )
            val feeSat = coins.map(_.amountSat).sum - tx.outputs.map(_.value.satoshis.toLong).sum
            require(
              feeSat <= TrafficPlan.FeeCapSat,
              s"Deposit fee $feeSat sat exceeds the cap of ${TrafficPlan.FeeCapSat} sat"
            )
            tx
        }
}
