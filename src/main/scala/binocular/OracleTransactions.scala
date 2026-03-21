package binocular

import binocular.util.SlotConfigHelper
import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, CardanoInfo, Coin, PlutusScript, Script, ScriptHash, ScriptRef, Transaction, TransactionOutput, Utxo, Utxos, Value}
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.uplc.builtin.Data
import binocular.OracleAction.*
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}
import scalus.utils.{await, showDetailedHighlighted, showHighlighted}

/** Helper functions for building oracle transactions */
object OracleTransactions {

    /** Build a transaction that deploys the oracle script to a UTxO as a reference script. */
    def buildDeployReferenceScript(
        signer: TransactionSigner,
        provider: BlockchainProvider,
        sponsorAddress: Address,
        destinationAddress: Address,
        script: Script
    )(using ExecutionContext): Future[(Transaction, TransactionOutput)] = {
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
        signer: TransactionSigner,
        provider: BlockchainProvider,
        sponsorAddress: Address,
        destinationAddress: Address,
        script: Script,
        timeout: Duration = 120.seconds
    ): Either[String, (String, Int, TransactionOutput)] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
            val (tx, output) = buildDeployReferenceScript(
              signer,
              provider,
              sponsorAddress,
              destinationAddress,
              script
            ).await(timeout)

            // Find actual output index (TxBuilder may reorder outputs)
            val scriptRefIdx = tx.body.value.outputs.indexWhere { sized =>
                sized.value.scriptRef.isDefined
            }
            val actualIdx = if scriptRefIdx >= 0 then scriptRefIdx else 0

            submitTx(provider, tx, timeout) match {
                case Right(txHash) => Right((txHash, actualIdx, output))
                case Left(err)     => Left(err)
            }
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

        println(
          s"Searching for reference script UTxOs at address: ${searchAddress.encode.getOrElse("?")}"
        )
        println(s"Expected script hash: ${expectedScriptHash.toHex}")

        val utxosResult = provider.findUtxos(searchAddress).await(timeout)

        val utxos: Utxos = utxosResult match {
            case Right(u)  => u
            case Left(err) => throw new RuntimeException(s"Failed to fetch UTxOs: $err")
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

        println(s"Found ${matchingUtxos.size} matching reference script UTxO(s)")
        matchingUtxos
    }

    /** Compute the validity interval start time that will be used on-chain. */
    def computeValidityIntervalTime(
        cardanoInfo: CardanoInfo,
        targetTimeSeconds: Option[BigInt] = None
    ): (Instant, BigInt) = {
        SlotConfigHelper.computeValidityIntervalTime(cardanoInfo, targetTimeSeconds)
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
        params: BitcoinValidatorParams
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

        // Use a large maxPromotions to find all promotable blocks
        val (promoted, _) = BitcoinValidator.promoteAndGC(
          newTree,
          ctx0,
          bestPath,
          bestDepth,
          currentTime,
          BigInt(1000), // large limit
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
        params: BitcoinValidatorParams
    ): (ChainState, ScalusList[ScalusList[ProofStep]], OffChainMPF) = {
        // 1. Determine which blocks will be promoted
        val promotedBlocks =
            computePromotedBlocks(currentState, headers, parentPath, currentTime, params)

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
        signer: TransactionSigner,
        provider: BlockchainProvider,
        scriptAddress: Address,
        sponsorAddress: Address,
        initialState: ChainState,
        script: Script.PlutusV3,
        oneShotUtxo: Utxo,
        referenceScriptUtxo: Utxo,
        lovelaceAmount: Long = 5_000_000L
    )(using ExecutionContext): Future[Transaction] = {
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

        println(scriptAddress)
        println(policyId)
        println(referenceScriptUtxo)
        TxBuilder(provider.cardanoInfo)
            .spend(oneShotUtxo) // consume one-shot UTxO for NFT policy
            .references(referenceScriptUtxo)
            .mint(policyId, mintAssets, mintRedeemer)
            .payTo(scriptAddress, nftValue, initialState)
            .complete(provider, sponsorAddress)
            .map(_.sign(signer).transaction)
    }

    /** Build and submit initialization transaction. */
    def buildAndSubmitInitTransaction(
        signer: TransactionSigner,
        provider: BlockchainProvider,
        scriptAddress: Address,
        sponsorAddress: Address,
        initialState: ChainState,
        script: Script.PlutusV3,
        oneShotUtxo: Utxo,
        referenceScriptUtxo: Utxo,
        lovelaceAmount: Long = 5_000_000L,
        timeout: Duration = 120.seconds
    ): Either[String, String] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
            val tx = buildInitTransaction(
              signer,
              provider,
              scriptAddress,
              sponsorAddress,
              initialState,
              script,
              oneShotUtxo,
              referenceScriptUtxo,
              lovelaceAmount
            ).await(timeout)

            import scalus.utils.*
            println(tx.showHighlighted)
            submitTx(provider, tx, timeout)
        } match {
            case Success(result) => result
            case Failure(ex) =>
                ex.printStackTrace()
                Left(
                  s"Error building transaction: ${ex.getMessage}\nCause: ${Option(ex.getCause).map(_.getMessage).getOrElse("none")}"
                )
        }
    }

    /** Build an UpdateOracle transaction. */
    def buildUpdateTransaction(
        signer: TransactionSigner,
        provider: BlockchainProvider,
        scriptAddress: Address,
        sponsorAddress: Address,
        oracleUtxo: Utxo,
        currentChainState: ChainState,
        newChainState: ChainState,
        blockHeaders: ScalusList[BlockHeader],
        parentPath: ScalusList[BigInt],
        validityIntervalTimeSeconds: BigInt,
        script: Script.PlutusV3,
        referenceScriptUtxo: Option[Utxo] = None,
        mpfInsertProofs: ScalusList[ScalusList[ProofStep]] = ScalusList.Nil
    )(using ExecutionContext): Future[Transaction] = {
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

        // Compute validity interval time from current time
        val (validityInstant, validatorWillSeeTime) =
            computeValidityIntervalTime(
              cardanoInfo,
              Some(validityIntervalTimeSeconds)
            )

        // Create UpdateOracle redeemer
        val redeemer = UpdateOracle(blockHeaders, parentPath, mpfInsertProofs)

        // Build the transaction
        var builder = TxBuilder(cardanoInfo)

        // Check if reference script UTxO actually contains the script
        val useRefScript = referenceScriptUtxo.exists(_.output.scriptRef.isDefined)

        // Add reference script if available and valid
        if useRefScript then {
            builder = builder.references(referenceScriptUtxo.get)
        }

        // Spend oracle UTxO with redeemer
        builder = if useRefScript then {
            builder.spend(oracleUtxo, redeemer)
        } else {
            builder.spend(oracleUtxo, redeemer, script)
        }

        // Output new state with same value
        // Validator requires finite validity interval <= MaxValidityWindow (10 min)
        val validToInstant =
            validityInstant.plusMillis(BitcoinValidator.MaxValidityWindow.toLong)
        builder = builder
            .payTo(scriptAddress, oracleUtxo.output.value, newChainState)
            .validFrom(validityInstant)
            .validTo(validToInstant)

        builder.complete(provider, sponsorAddress).map(_.sign(signer).transaction)
    }

    /** Build and submit UpdateOracle transaction */
    def buildAndSubmitUpdateTransaction(
        signer: TransactionSigner,
        provider: BlockchainProvider,
        scriptAddress: Address,
        sponsorAddress: Address,
        oracleUtxo: Utxo,
        currentChainState: ChainState,
        newChainState: ChainState,
        blockHeaders: ScalusList[BlockHeader],
        parentPath: ScalusList[BigInt],
        validityIntervalTimeSeconds: BigInt,
        script: Script.PlutusV3,
        referenceScriptUtxo: Option[Utxo] = None,
        timeout: Duration = 120.seconds,
        mpfInsertProofs: ScalusList[ScalusList[ProofStep]] = ScalusList.Nil
    ): Either[String, String] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
            val tx = buildUpdateTransaction(
              signer,
              provider,
              scriptAddress,
              sponsorAddress,
              oracleUtxo,
              currentChainState,
              newChainState,
              blockHeaders,
              parentPath,
              validityIntervalTimeSeconds,
              script,
              referenceScriptUtxo,
              mpfInsertProofs
            ).await(timeout)

            submitTx(provider, tx, timeout)
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
        signer: TransactionSigner,
        provider: BlockchainProvider,
        scriptAddress: Address,
        sponsorAddress: Address,
        oracleUtxo: Utxo,
        script: Script.PlutusV3,
        referenceScriptUtxo: Option[Utxo] = None
    )(using ExecutionContext): Future[Transaction] = {
        val cardanoInfo = provider.cardanoInfo

        val (validityInstant, _) =
            computeValidityIntervalTime(cardanoInfo)

        val redeemer = OracleAction.CloseOracle

        var builder = TxBuilder(cardanoInfo)

        val useRefScript = referenceScriptUtxo.exists(_.output.scriptRef.isDefined)
        if useRefScript then {
            builder = builder.references(referenceScriptUtxo.get)
        }

        builder = if useRefScript then {
            builder.spend(oracleUtxo, redeemer)
        } else {
            builder.spend(oracleUtxo, redeemer, script)
        }

        // Burn the NFT (quantity -1)
        val burnAssets = Map(scalus.cardano.ledger.AssetName.empty -> -1L)
        builder = builder.mint(script, burnAssets, redeemer)

        val validToInstant =
            validityInstant.plusMillis(BitcoinValidator.MaxValidityWindow.toLong)
        builder = builder
            .payTo(sponsorAddress, Value(oracleUtxo.output.value.coin))
            .validFrom(validityInstant)
            .validTo(validToInstant)

        builder.complete(provider, sponsorAddress).map(_.sign(signer).transaction)
    }

    /** Build and submit CloseOracle transaction to close a stale oracle and burn the NFT. */
    def buildAndSubmitCloseTransaction(
        signer: TransactionSigner,
        provider: BlockchainProvider,
        scriptAddress: Address,
        sponsorAddress: Address,
        oracleUtxo: Utxo,
        script: Script.PlutusV3,
        referenceScriptUtxo: Option[Utxo] = None,
        timeout: Duration = 120.seconds
    ): Either[String, String] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
            val tx = buildCloseTransaction(
              signer,
              provider,
              scriptAddress,
              sponsorAddress,
              oracleUtxo,
              script,
              referenceScriptUtxo
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
        signer: TransactionSigner,
        provider: BlockchainProvider,
        sponsorAddress: Address,
        lovelaceAmount: Long = 10_000_000L,
        timeout: Duration = 120.seconds
    ): Either[String, (String, Int, TransactionOutput)] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
            val tx = TxBuilder(provider.cardanoInfo)
                .payTo(address = sponsorAddress, value = Value.lovelace(lovelaceAmount))
                .minFee(Coin.ada(1)) // FIXME: why?
                .complete(provider, sponsorAddress)
                .map(_.sign(signer).transaction)
                .await(timeout)

            println(tx.showDetailedHighlighted)

            submitTx(provider, tx, timeout) match {
                case Right(txHash) => Right((txHash, 0, tx.body.value.outputs.head.value))
                case Left(err)     => Left(err)
            }
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
                case Left(err) =>
                    println(s"  Waiting for ${input.transactionId.toHex}#${input.index}: $err")
                    Thread.sleep(pollInterval.toMillis)
            }
        }
        Left(s"Timeout waiting for UTxO ${input.transactionId.toHex}#${input.index}")
    }
}
