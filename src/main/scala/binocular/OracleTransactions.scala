package binocular

import binocular.util.SlotConfigHelper
import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, CardanoInfo, PlutusScript, ScriptHash, ScriptRef, Transaction, TransactionHash, TransactionOutput, Utxo, Utxos, Value}
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.txbuilder.TxBuilder
import scalus.uplc.builtin.Data
import binocular.OracleAction.*
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.ledger.rules.ExUnitsTooBigValidator
import scalus.cardano.wallet.hd.HdAccount
import scalus.uplc.CompiledPlutus

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}
import scalus.utils.await
import cats.syntax.either.*

/** Result of a submitted transaction with size metrics. */
case class TxResult(txHash: String, txSize: Int, datumSize: Int)

/** Helper functions for building oracle transactions */
object OracleTransactions {

    /** Build a transaction that deploys the oracle script to a UTxO as a reference script. */
    def buildDeployReferenceScript(
        provider: BlockchainProvider,
        sponsor: HdAccount,
        compiled: CompiledPlutus[?]
    )(using ExecutionContext): Future[(Transaction, TransactionOutput)] = {
        val network = provider.cardanoInfo.network
        val signer = sponsor.signerForUtxos
        val sponsorAddress = sponsor.baseAddress(network)
        val destinationAddress = compiled.address(network)
        val script = compiled.script

        val output = TransactionOutput(
          address = destinationAddress,
          value = Value.lovelace(50_000_000), // 50 ADA
          datumOption = None,
          scriptRef = Some(ScriptRef(script))
        )

        TxBuilder(provider.cardanoInfo)
            .output(output)
            .complete(provider, sponsorAddress)
            .map { completed =>
                (completed.sign(signer).transaction, output)
            }
    }

    /** Deploy the oracle script to a UTxO as a reference script. */
    def deployReferenceScript(
        provider: BlockchainProvider,
        sponsor: HdAccount,
        compiled: CompiledPlutus[?],
        timeout: Duration = 120.seconds
    ): Either[String, (String, Int, TransactionOutput)] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
            val (tx, output) = buildDeployReferenceScript(
              provider,
              sponsor,
              compiled
            ).await(timeout)

            // Find actual output index (TxBuilder may reorder outputs)
            val scriptRefIdx = tx.body.value.outputs.indexWhere { sized =>
                sized.value.scriptRef.isDefined
            }
            val actualIdx = if scriptRefIdx >= 0 then scriptRefIdx else 0

            submitTx(provider, tx, timeout).map((_, actualIdx, output))
        } match {
            case Success(result) => result
            case Failure(ex) =>
                Left(s"Error deploying reference script: ${ex.getMessage}")
        }
    }

    /** Find existing reference script UTxOs at a given address */
    def findReferenceScriptUtxos(
        provider: BlockchainProvider,
        searchAddress: Address,
        expectedScriptHash: ScriptHash,
        timeout: Duration = 30.seconds
    ): List[(String, Int)] = {
        given ec: ExecutionContext = provider.executionContext
        val utxosResult = provider.findUtxos(searchAddress).await(timeout)

        val utxos: Utxos = utxosResult.valueOr { err =>
            throw new RuntimeException(s"Failed to fetch UTxOs: $err")
        }

        val matchingUtxos = utxos.toList
            .filter { case (input, output) =>
                output.scriptRef.exists { ref =>
                    ref.script match {
                        case ps: PlutusScript => ps.scriptHash == expectedScriptHash
                        case _                => false
                    }
                }
            }
            .map { case (input, _) =>
                (input.transactionId.toHex, input.index)
            }

        matchingUtxos
    }

    /** Compute the validity interval for an oracle transaction.
      *
      * Returns `(validFromInstant, onChainEndTimeSeconds)`:
      *
      *   - `validFromInstant` is the wall-clock start of the validity window — pass directly to
      *     `TxBuilder.validFrom(...)`. The caller adds `MaxValidityWindow` to get `validTo`.
      *   - `onChainEndTimeSeconds` is the **end** of the validity window in seconds. The on-chain
      *     validator reads `tx.validRange.to` as its notion of "current time", so this is the value
      *     that must flow into `BitcoinValidator.computeUpdate` as `currentTime`. See the extended
      *     rationale in `BitcoinValidator.spend`.
      */
    def computeValidityIntervalTime(
        cardanoInfo: CardanoInfo,
        targetEndTimeSeconds: Option[BigInt] = None
    ): (Instant, BigInt) = {
        SlotConfigHelper.computeValidityIntervalTime(cardanoInfo, targetEndTimeSeconds)
    }

    /** Apply Bitcoin headers to ChainState to calculate new state. Uses empty MPF proofs - only
      * works when no promotion occurs.
      */
    def applyHeaders(
        currentState: ChainState,
        headers: ScalusList[BlockHeader],
        parentPath: ScalusList[BigInt],
        currentTime: BigInt,
        params: BitcoinValidatorParams
    ): ChainState = {
        BitcoinValidator.computeUpdate(
          currentState,
          headers,
          parentPath,
          ScalusList.Nil,
          currentTime,
          params
        )
    }

    /** Determine which blocks will be promoted when applying headers to the state. */
    def computePromotedBlocks(
        prevState: ChainState,
        blockHeaders: ScalusList[BlockHeader],
        parentPath: ScalusList[BigInt],
        currentTime: BigInt,
        params: BitcoinValidatorParams,
        maxPromotions: Int = 1000
    ): ScalusList[BlockSummary] = {
        val ctx0 = prevState.ctx

        // Insert headers into fork tree
        val newTree = blockHeaders match
            case ScalusList.Nil => prevState.forkTree
            case _ =>
                BitcoinValidator.validateAndInsert(
                  prevState.forkTree,
                  parentPath,
                  blockHeaders,
                  ctx0,
                  currentTime,
                  params
                )

        // Find best chain path and promote
        val (_, bestDepth, bestPath) =
            BitcoinValidator.bestChainPath(newTree, prevState.ctx.height, 0)

        val (promoted, _) = BitcoinValidator.promoteAndGC(
          newTree,
          ctx0,
          bestPath,
          bestDepth,
          currentTime,
          BigInt(maxPromotions),
          params
        )
        promoted
    }

    /** Compute new ChainState with real MPF insert proofs. */
    def computeUpdateWithProofs(
        currentState: ChainState,
        headers: ScalusList[BlockHeader],
        parentPath: ScalusList[BigInt],
        currentTime: BigInt,
        offChainMpf: OffChainMPF,
        params: BitcoinValidatorParams,
        maxPromotions: Int = 1000
    ): (ChainState, ScalusList[ScalusList[ProofStep]], OffChainMPF) = {
        // 1. Determine which blocks will be promoted
        val promotedBlocks =
            computePromotedBlocks(
              currentState,
              headers,
              parentPath,
              currentTime,
              params,
              maxPromotions
            )

        // 2. Generate MPF insert proofs for each promoted block.
        var mpf = offChainMpf
        val proofsBuilder = scala.collection.mutable.ListBuffer[ScalusList[ProofStep]]()

        promotedBlocks.foreach { block =>
            val proof = mpf.proveNonMembership(block.hash)
            proofsBuilder += proof
            mpf = mpf.insert(block.hash, block.hash)
        }

        val mpfProofs = ScalusList.from(proofsBuilder.toList)

        // 3. Compute the full state with real proofs
        val newState = BitcoinValidator.computeUpdate(
          currentState,
          headers,
          parentPath,
          mpfProofs,
          currentTime,
          params
        )

        (newState, mpfProofs, mpf)
    }

    /** Build the initialization transaction.
      *
      * Mints the oracle NFT (one-shot policy) and sends it to the script address with the initial
      * ChainState as inline datum. The one-shot UTxO (from params.oneShotTxOutRef) must be
      * consumed. Uses the reference script UTxO for the minting policy.
      */
    def buildInitTransaction(
        provider: BlockchainProvider,
        sponsor: HdAccount,
        compiled: CompiledPlutus[?],
        initialState: ChainState,
        oneShotUtxo: Utxo,
        referenceScriptUtxo: Utxo,
        lovelaceAmount: Long = 5_000_000L
    )(using ExecutionContext): Future[Transaction] = {
        val network = provider.cardanoInfo.network
        val signer = sponsor.signerForUtxos
        val sponsorAddress = sponsor.baseAddress(network)
        val scriptAddress = compiled.address(network)
        val script = compiled.script
        val policyId = script.scriptHash
        val nftValue =
            Value.lovelace(lovelaceAmount) + Value.asset(policyId, AssetName.empty, 1L)

        // Mint redeemer: output index of the oracle output in the final transaction.
        // The minting policy validates that the NFT goes to the script address at this index.
        val mintRedeemer: Transaction => Data = { tx =>
            val idx = tx.body.value.outputs.indexWhere { sized =>
                sized.value.value.hasAsset(policyId, AssetName.empty)
            }
            Data.I(idx)
        }

        val mintAssets = Map(AssetName.empty -> 1L)

        TxBuilder(provider.cardanoInfo)
            .spend(oneShotUtxo) // consume one-shot UTxO for NFT policy
            .references(referenceScriptUtxo, compiled)
            .mint(policyId, mintAssets, mintRedeemer)
            .payTo(scriptAddress, nftValue, initialState)
            .complete(provider, sponsorAddress)
            .map(_.sign(signer).transaction)
    }

    /** Build and submit initialization transaction. */
    def buildAndSubmitInitTransaction(
        provider: BlockchainProvider,
        sponsor: HdAccount,
        compiled: CompiledPlutus[?],
        initialState: ChainState,
        oneShotUtxo: Utxo,
        referenceScriptUtxo: Utxo,
        lovelaceAmount: Long = 5_000_000L,
        timeout: Duration = 120.seconds
    ): Either[String, TxResult] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
            val tx = buildInitTransaction(
              provider,
              sponsor,
              compiled,
              initialState,
              oneShotUtxo,
              referenceScriptUtxo,
              lovelaceAmount
            ).await(timeout)

            val txSize = tx.toCbor.length
            val datumSize = tx.body.value.outputs
                .find(_.value.inlineDatum.isDefined)
                .map(_.value.requireInlineDatum.toCbor.length)
                .getOrElse(0)

            submitTx(provider, tx, timeout).map(hash => TxResult(hash, txSize, datumSize))
        } match {
            case Success(result) => result
            case Failure(ex) =>
                Left(
                  s"Error building transaction: ${ex.getMessage}\nCause: ${Option(ex.getCause).map(_.getMessage).getOrElse("none")}"
                )
        }
    }

    /** Build an UpdateOracle transaction. Uses reference script for spending. */
    def buildUpdateTransaction(
        provider: BlockchainProvider,
        sponsor: HdAccount,
        compiled: CompiledPlutus[?],
        oracleUtxo: Utxo,
        currentChainState: ChainState,
        newChainState: ChainState,
        blockHeaders: ScalusList[BlockHeader],
        parentPath: ScalusList[BigInt],
        validityIntervalTimeSeconds: BigInt,
        referenceScriptUtxo: Utxo,
        mpfInsertProofs: ScalusList[ScalusList[ProofStep]] = ScalusList.Nil
    )(using ExecutionContext): Future[Transaction] = {
        val network = provider.cardanoInfo.network
        val signer = sponsor.signerForUtxos
        val sponsorAddress = sponsor.baseAddress(network)
        val scriptAddress = compiled.address(network)
        // Verify that the input UTxO's datum matches currentChainState
        val inputData = oracleUtxo.output.requireInlineDatum
        val inputState = inputData.to[ChainState]

        if inputState.ctx.height != currentChainState.ctx.height ||
            inputState.ctx.lastBlockHash != currentChainState.ctx.lastBlockHash
        then {
            throw new RuntimeException(
              s"Input UTxO state does not match provided currentChainState!\n" +
                  s"  Provided currentChainState: height=${currentChainState.ctx.height}, hash=${currentChainState.ctx.lastBlockHash.toHex}\n" +
                  s"  Input UTxO state: height=${inputState.ctx.height}, hash=${inputState.ctx.lastBlockHash.toHex}"
            )
        }

        val cardanoInfo = provider.cardanoInfo

        // Reconstruct the wall-clock validFrom from the previously-computed on-chain end time.
        val (validityInstant, _) =
            computeValidityIntervalTime(
              cardanoInfo,
              Some(validityIntervalTimeSeconds)
            )

        // Create UpdateOracle redeemer
        val redeemer = UpdateOracle(blockHeaders, parentPath, mpfInsertProofs)

        // Validator requires finite validity interval <= MaxValidityWindow (10 min)
        val validToInstant =
            validityInstant.plusMillis(BitcoinValidator.MaxValidityWindow.toLong)

        TxBuilder(cardanoInfo)
            .references(referenceScriptUtxo, compiled)
            .spend(oracleUtxo, redeemer)
            .payTo(scriptAddress, oracleUtxo.output.value, newChainState)
            .validFrom(validityInstant)
            .validTo(validToInstant)
            .complete(provider, sponsorAddress)
            .map(_.sign(signer).transaction)
    }

    /** Build an UpdateOracle TxBuilder synchronously using pre-fetched sponsor UTxOs.
      *
      * This avoids async provider calls, making it suitable for repeated calls during binary
      * search.
      */
    private def buildUpdateTxBuilder(
        cardanoInfo: CardanoInfo,
        compiled: CompiledPlutus[?],
        scriptAddress: Address,
        sponsorAddress: Address,
        sponsorUtxos: Utxos,
        oracleUtxo: Utxo,
        currentChainState: ChainState,
        newChainState: ChainState,
        blockHeaders: ScalusList[BlockHeader],
        parentPath: ScalusList[BigInt],
        validityIntervalTimeSeconds: BigInt,
        referenceScriptUtxo: Utxo,
        mpfInsertProofs: ScalusList[ScalusList[ProofStep]]
    ): TxBuilder = {
        val (validityInstant, _) =
            computeValidityIntervalTime(cardanoInfo, Some(validityIntervalTimeSeconds))

        val redeemer = UpdateOracle(blockHeaders, parentPath, mpfInsertProofs)

        val validToInstant =
            validityInstant.plusMillis(BitcoinValidator.MaxValidityWindow.toLong)

        TxBuilder(cardanoInfo)
            .references(referenceScriptUtxo, compiled)
            .spend(oracleUtxo, redeemer)
            .payTo(scriptAddress, oracleUtxo.output.value, newChainState)
            .validFrom(validityInstant)
            .validTo(validToInstant)
            .complete(sponsorUtxos, sponsorAddress)
    }

    /** Binary-search for the maximum number of promotions that fit in a valid transaction.
      *
      * Returns the signed transaction result, updated chain state, updated off-chain MPF, and the
      * number of promotions performed.
      */
    def buildOptimalUpdateTransaction(
        provider: BlockchainProvider,
        sponsor: HdAccount,
        compiled: CompiledPlutus[?],
        oracleUtxo: Utxo,
        currentChainState: ChainState,
        blockHeaders: ScalusList[BlockHeader],
        parentPath: ScalusList[BigInt],
        validityIntervalTimeSeconds: BigInt,
        referenceScriptUtxo: Utxo,
        offChainMpf: OffChainMPF,
        params: BitcoinValidatorParams,
        timeout: Duration
    )(using ExecutionContext): Either[String, (TxResult, ChainState, OffChainMPF, Int)] = {
        Try {
            val cardanoInfo = provider.cardanoInfo
            val protocolParams = cardanoInfo.protocolParams
            val network = cardanoInfo.network
            val signer = sponsor.signerForUtxos
            val sponsorAddress = sponsor.baseAddress(network)
            val scriptAddress = compiled.address(network)

            // Pre-fetch sponsor UTxOs once
            val sponsorUtxos = provider.findUtxos(sponsorAddress).await(timeout).valueOr { err =>
                throw new RuntimeException(s"Failed to fetch sponsor UTxOs: $err")
            }

            // Find total promotable blocks
            val totalPromotable = computePromotedBlocks(
              currentChainState,
              blockHeaders,
              parentPath,
              validityIntervalTimeSeconds,
              params
            ).length

            val maxTxSize = protocolParams.maxTxSize

            // Try building with a given number of promotions.
            // Signs the transaction and checks actual CBOR size against protocol limits.
            def tryBuild(
                maxPromotions: Int
            ): Option[(Transaction, ChainState, OffChainMPF)] = {
                val (newState, mpfProofs, updatedMpf) =
                    computeUpdateWithProofs(
                      currentChainState,
                      blockHeaders,
                      parentPath,
                      validityIntervalTimeSeconds,
                      offChainMpf,
                      params,
                      maxPromotions
                    )

                // Skip if no state change
                if newState == currentChainState then return None

                try {
                    val builder = buildUpdateTxBuilder(
                      cardanoInfo,
                      compiled,
                      scriptAddress,
                      sponsorAddress,
                      sponsorUtxos,
                      oracleUtxo,
                      currentChainState,
                      newState,
                      blockHeaders,
                      parentPath,
                      validityIntervalTimeSeconds,
                      referenceScriptUtxo,
                      mpfProofs
                    )

                    // Validate ExUnits against protocol limits
                    builder.context
                        .validate(Seq(ExUnitsTooBigValidator), protocolParams) match {
                        case Left(_) => return None
                        case _       => ()
                    }

                    // Sign and check actual CBOR size (signing adds witness bytes)
                    val tx = builder.sign(signer).transaction
                    if tx.toCbor.length > maxTxSize then None
                    else Some((tx, newState, updatedMpf))
                } catch {
                    case _: Exception => None
                }
            }

            // Binary search for max promotions
            var low = 0
            var high = totalPromotable.toInt
            var best: Option[(Transaction, ChainState, OffChainMPF, Int)] = None

            while low <= high do {
                val mid = (low + high) / 2
                tryBuild(mid) match {
                    case Some((tx, newState, updatedMpf)) =>
                        best = Some((tx, newState, updatedMpf, mid))
                        low = mid + 1
                    case None =>
                        high = mid - 1
                }
            }

            // Fall back to 0 promotions if no promotion count worked
            val (tx, newState, updatedMpf, promotionCount) = best.getOrElse {
                tryBuild(0) match {
                    case Some((t, ns, um)) => (t, ns, um, 0)
                    case None =>
                        throw new RuntimeException(
                          "Failed to build transaction even with 0 promotions"
                        )
                }
            }
            val txSize = tx.toCbor.length
            val datumSize = tx.body.value.outputs
                .find(_.value.inlineDatum.isDefined)
                .map(_.value.requireInlineDatum.toCbor.length)
                .getOrElse(0)

            submitTx(provider, tx, timeout)
                .map(hash =>
                    (TxResult(hash, txSize, datumSize), newState, updatedMpf, promotionCount)
                )
        } match {
            case Success(result) => result
            case Failure(ex) =>
                Left(
                  s"Error building optimal UpdateOracle transaction: ${ex.getMessage}\n${ex.getStackTrace.take(5).mkString("\n")}"
                )
        }
    }

    /** Build and submit UpdateOracle transaction */
    def buildAndSubmitUpdateTransaction(
        provider: BlockchainProvider,
        sponsor: HdAccount,
        compiled: CompiledPlutus[?],
        oracleUtxo: Utxo,
        currentChainState: ChainState,
        newChainState: ChainState,
        blockHeaders: ScalusList[BlockHeader],
        parentPath: ScalusList[BigInt],
        validityIntervalTimeSeconds: BigInt,
        referenceScriptUtxo: Utxo,
        timeout: Duration = 120.seconds,
        mpfInsertProofs: ScalusList[ScalusList[ProofStep]] = ScalusList.Nil
    ): Either[String, TxResult] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
            val tx = buildUpdateTransaction(
              provider,
              sponsor,
              compiled,
              oracleUtxo,
              currentChainState,
              newChainState,
              blockHeaders,
              parentPath,
              validityIntervalTimeSeconds,
              referenceScriptUtxo,
              mpfInsertProofs
            ).await(timeout)

            val txSize = tx.toCbor.length
            val datumSize = tx.body.value.outputs
                .find(_.value.inlineDatum.isDefined)
                .map(_.value.requireInlineDatum.toCbor.length)
                .getOrElse(0)

            submitTx(provider, tx, timeout).map(hash => TxResult(hash, txSize, datumSize))
        } match {
            case Success(result) => result
            case Failure(ex) =>
                Left(
                  s"Error building UpdateOracle transaction: ${ex.getMessage}\n${ex.getStackTrace.take(5).mkString("\n")}"
                )
        }
    }

    /** Build a CloseOracle transaction to close a stale oracle and burn the NFT. */
    def buildCloseTransaction(
        provider: BlockchainProvider,
        owner: HdAccount,
        oracleUtxo: Utxo,
        referenceScriptUtxo: Utxo,
        compiled: CompiledPlutus[?]
    )(using ExecutionContext): Future[Transaction] = {
        val cardanoInfo = provider.cardanoInfo
        val network = cardanoInfo.network
        val sponsorAddress = owner.baseAddress(network)
        val signer = owner.signerForUtxos

        val (validityInstant, _) =
            computeValidityIntervalTime(cardanoInfo)

        val redeemer = OracleAction.CloseOracle

        // Burn the NFT (quantity -1)
        val policyId = compiled.script.scriptHash
        val burnAssets = Map(AssetName.empty -> -1L)

        val validToInstant =
            validityInstant.plusMillis(BitcoinValidator.MaxValidityWindow.toLong)

        TxBuilder(cardanoInfo)
            .references(referenceScriptUtxo, compiled)
            .spend(oracleUtxo, redeemer)
            .requireSignatures(Set(owner.paymentKeyHash))
            .mint(policyId, burnAssets, redeemer)
            .payTo(sponsorAddress, Value(oracleUtxo.output.value.coin))
            .validFrom(validityInstant)
            .validTo(validToInstant)
            .complete(provider, sponsorAddress)
            .map(_.sign(signer).transaction)
    }

    /** Build and submit CloseOracle transaction to close a stale oracle and burn the NFT. */
    def buildAndSubmitCloseTransaction(
        provider: BlockchainProvider,
        owner: HdAccount,
        oracleUtxo: Utxo,
        referenceScriptUtxo: Utxo,
        compiled: CompiledPlutus[?],
        timeout: Duration = 120.seconds
    ): Either[String, String] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
            val tx = buildCloseTransaction(
              provider,
              owner,
              oracleUtxo,
              referenceScriptUtxo,
              compiled
            ).await(timeout)

            submitTx(provider, tx, timeout)
        } match {
            case Success(result) => result
            case Failure(ex) =>
                Left(
                  s"Error building CloseOracle transaction: ${ex.getMessage}\n${ex.getStackTrace.take(5).mkString("\n")}"
                )
        }
    }

    /** Submit a transaction and return the hash */
    def submitTx(
        provider: BlockchainProvider,
        tx: Transaction,
        timeout: Duration = 120.seconds
    ): Either[String, String] = {
        given ec: ExecutionContext = provider.executionContext
        val result = provider.submit(tx).await(timeout)
        result match {
            case Left(err)     => Left(s"Submission failed: $err")
            case Right(txHash) => Right(txHash.toHex)
        }
    }

    /** Create a dedicated one-shot UTxO by paying to sponsor address.
      *
      * Returns the tx hash, output index, and output of the created UTxO.
      */
    def createOneShotUtxo(
        provider: BlockchainProvider,
        sponsor: HdAccount,
        lovelaceAmount: Long = 10_000_000L,
        timeout: Duration = 120.seconds
    ): Either[String, (String, Int, TransactionOutput)] = {
        given ec: ExecutionContext = provider.executionContext
        val signer = sponsor.signerForUtxos
        val sponsorAddress = sponsor.baseAddress(provider.cardanoInfo.network)
        Try {
            val tx = TxBuilder(provider.cardanoInfo)
                .payTo(address = sponsorAddress, value = Value.lovelace(lovelaceAmount))
                .complete(provider, sponsorAddress)
                .map(_.sign(signer).transaction)
                .await(timeout)

            submitTx(provider, tx, timeout).map((_, 0, tx.body.value.outputs.head.value))
        } match {
            case Success(result) => result
            case Failure(ex) =>
                Left(s"Error creating one-shot UTxO: ${ex.getMessage}")
        }
    }

    /** Wait for a UTxO to appear on-chain by polling the provider. */
    def waitForUtxo(
        provider: BlockchainProvider,
        input: scalus.cardano.ledger.TransactionInput,
        timeout: Duration = 120.seconds,
        pollInterval: Duration = 1.second
    )(using ExecutionContext): Either[String, Utxo] = {
        val deadline = System.currentTimeMillis() + timeout.toMillis
        while System.currentTimeMillis() < deadline do {
            provider.findUtxo(input).await(10.seconds) match {
                case Right(utxo) => return Right(utxo)
                case Left(_) =>
                    Thread.sleep(pollInterval.toMillis)
            }
        }
        Left(s"Timeout waiting for UTxO ${input.transactionId.toHex}#${input.index}")
    }

    /** Wait until an output from a given transaction appears in the address-based UTxO query.
      *
      * After a transaction is confirmed, [[waitForUtxo]] (which uses `findUtxo` by outref) may
      * succeed while `findUtxos(address)` still returns stale results. This causes subsequent
      * transactions built via `TxBuilder.complete(provider, address)` to use already-spent inputs.
      *
      * Call this between sequential transactions that share a sponsor address to ensure the
      * address-based index has caught up.
      */
    def waitForUtxoAtAddress(
        provider: BlockchainProvider,
        address: Address,
        expectedTxHash: TransactionHash,
        timeout: Duration = 120.seconds,
        pollInterval: Duration = 1.second
    )(using ExecutionContext): Either[String, Unit] = {
        val deadline = System.currentTimeMillis() + timeout.toMillis
        while System.currentTimeMillis() < deadline do {
            provider.findUtxos(address).await(10.seconds) match {
                case Right(utxos) if utxos.keys.exists(_.transactionId == expectedTxHash) =>
                    return Right(())
                case _ =>
                    Thread.sleep(pollInterval.toMillis)
            }
        }
        Left(s"Timeout waiting for tx ${expectedTxHash.toHex} output at address")
    }
}
