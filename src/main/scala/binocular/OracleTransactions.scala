package binocular

import binocular.util.SlotConfigHelper
import scalus.cardano.address.Address
import scalus.cardano.ledger.{CardanoInfo, PlutusScript, Script, ScriptRef, Transaction, TransactionOutput, Utxo, Utxos, Value}
import scalus.cardano.onchain.plutus.v3.TxOutRef
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.uplc.builtin.Data
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep

import java.time.Instant
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

/** Helper functions for building oracle transactions */
object OracleTransactions {

    /** Get compiled PlutusV3 script for a specific TxOutRef parameter */
    def getPlutusScript(txOutRef: TxOutRef): Script.PlutusV3 = {
        Script.PlutusV3(BitcoinContract.makeScript(txOutRef).cborByteString)
    }

    /** Get compiled PlutusV3 script for specific BitcoinValidatorParams */
    def getPlutusScript(params: BitcoinValidatorParams): Script.PlutusV3 = {
        Script.PlutusV3(BitcoinContract.makeScript(params).cborByteString)
    }

    /** Deploy the oracle script to a UTxO as a reference script. */
    def deployReferenceScript(
        signer: TransactionSigner,
        provider: BlockchainProvider,
        sponsorAddress: Address,
        destinationAddress: Address,
        oracleTxOutRef: TxOutRef,
        timeout: Duration = 120.seconds
    ): Either[String, (String, Int, TransactionOutput)] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
            val script = getPlutusScript(oracleTxOutRef)

            val output = TransactionOutput.Babbage(
              address = destinationAddress,
              value = Value.lovelace(50_000_000), // 50 ADA
              datumOption = None,
              scriptRef = Some(ScriptRef(script))
            )

            val tx = Await
                .result(
                  TxBuilder(provider.cardanoInfo)
                      .output(output)
                      .complete(provider, sponsorAddress),
                  timeout
                )
                .sign(signer)
                .transaction

            submitTx(provider, tx, timeout) match {
                case Right(txHash) => Right((txHash, 0, output))
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
        oracleTxOutRef: TxOutRef,
        timeout: Duration = 30.seconds
    ): List[(String, Int)] = {
        given ec: ExecutionContext = provider.executionContext
        val script = getPlutusScript(oracleTxOutRef)
        val expectedScriptHash = script.scriptHash

        println(
          s"Searching for reference script UTxOs at address: ${searchAddress.encode.getOrElse("?")}"
        )
        println(s"Expected script hash: ${expectedScriptHash.toHex}")

        val utxosResult = Await.result(
          provider.findUtxos(searchAddress),
          timeout
        )

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
          UpdateOracle(headers, parentPath, ScalusList.Nil),
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
        val ctx0 = BitcoinValidator.initCtx(prevState)

        // Insert headers into fork tree
        val newTree = blockHeaders match
            case ScalusList.Nil => prevState.forkTree
            case _ =>
                BitcoinValidator.validateAndInsert(
                  prevState.forkTree,
                  parentPath,
                  blockHeaders,
                  ctx0,
                  currentTime
                )

        // Find best chain path and promote
        val (_, bestDepth, bestPath) =
            BitcoinValidator.bestChainPath(newTree, prevState.blockHeight, 0)

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
          UpdateOracle(headers, parentPath, mpfProofs),
          currentTime,
          params
        )

        (newState, mpfProofs, mpf)
    }

    /** Build and submit initialization transaction */
    def buildAndSubmitInitTransaction(
        signer: TransactionSigner,
        provider: BlockchainProvider,
        scriptAddress: Address,
        sponsorAddress: Address,
        initialState: ChainState,
        lovelaceAmount: Long = 5_000_000L,
        timeout: Duration = 120.seconds
    ): Either[String, String] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
            val tx = Await
                .result(
                  TxBuilder(provider.cardanoInfo)
                      .payTo(scriptAddress, Value.lovelace(lovelaceAmount), initialState)
                      .complete(provider, sponsorAddress),
                  timeout
                )
                .sign(signer)
                .transaction

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
        oracleTxOutRef: TxOutRef,
        referenceScriptUtxo: Option[Utxo] = None,
        timeout: Duration = 120.seconds,
        mpfInsertProofs: ScalusList[ScalusList[ProofStep]] = ScalusList.Nil
    ): Either[String, String] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
            val script = getPlutusScript(oracleTxOutRef)

            // Verify that the input UTxO's datum matches currentChainState
            val inputData = oracleUtxo.output.requireInlineDatum
            val inputState = inputData.to[ChainState]

            if inputState.blockHeight != currentChainState.blockHeight ||
                inputState.blockHash != currentChainState.blockHash
            then {
                throw new RuntimeException(
                  s"Input UTxO state does not match provided currentChainState!\n" +
                      s"  Provided currentChainState: height=${currentChainState.blockHeight}, hash=${currentChainState.blockHash.toHex}\n" +
                      s"  Input UTxO state: height=${inputState.blockHeight}, hash=${inputState.blockHash.toHex}"
                )
            }

            println(s"[DEBUG] Verified input UTXO datum matches currentChainState")

            val cardanoInfo = provider.cardanoInfo
            val slotConfig = cardanoInfo.slotConfig

            // Compute validity interval time from current time
            val (validityInstant, validatorWillSeeTime) =
                computeValidityIntervalTime(cardanoInfo, Some(validityIntervalTimeSeconds))

            println(s"[DEBUG] Computing time that validator will see:")
            println(s"  validatorWillSeeTime (seconds): $validatorWillSeeTime")

            // Create UpdateOracle redeemer
            val redeemer = UpdateOracle(blockHeaders, parentPath, mpfInsertProofs)

            println(
              s"[DEBUG] New state forkTree block count: ${newChainState.forkTree.blockCount}"
            )

            // Build the transaction
            var builder = TxBuilder(cardanoInfo)

            // Check if reference script UTxO actually contains the script
            val useRefScript = referenceScriptUtxo.exists(_.output.scriptRef.isDefined)

            // Add reference script if available and valid
            if useRefScript then {
                val refUtxo = referenceScriptUtxo.get
                println(
                  s"[DEBUG] Using reference script from UTxO: ${refUtxo.input.transactionId.toHex}:${refUtxo.input.index}"
                )
                builder = builder.references(refUtxo)
            } else {
                if referenceScriptUtxo.isDefined then
                    println(
                      s"[DEBUG] Reference UTxO has no scriptRef, falling back to attached script"
                    )
                else println(s"[DEBUG] No reference script UTxO, attaching script directly")
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

            val tx = Await
                .result(
                  builder.complete(provider, sponsorAddress),
                  timeout
                )
                .sign(signer)
                .transaction

            submitTx(provider, tx, timeout)
        } match {
            case Success(result) => result
            case Failure(ex) =>
                Left(
                  s"Error building UpdateOracle transaction: ${ex.getMessage}\n${ex.getStackTrace.take(5).mkString("\n")}"
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
        val result = Await.result(provider.submit(tx), timeout)
        result match {
            case Left(err)     => Left(s"Submission failed: $err")
            case Right(txHash) => Right(txHash.toHex)
        }
    }
}
