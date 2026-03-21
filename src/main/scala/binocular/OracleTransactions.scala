package binocular

import binocular.util.SlotConfigHelper
import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, CardanoInfo, PlutusScript, Script, ScriptHash, ScriptRef, Transaction, TransactionOutput, Utxo, Utxos, Value}
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.uplc.builtin.Data
import binocular.OracleAction.*
import scalus.cardano.onchain.plutus.prelude.List as ScalusList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep

import java.time.Instant
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}
import scalus.utils.await

/** Helper functions for building oracle transactions */
object OracleTransactions {

    /** Deploy the oracle script to a UTxO as a reference script. */
    def deployReferenceScript(
        signer: TransactionSigner,
        provider: BlockchainProvider,
        sponsorAddress: Address,
        destinationAddress: Address,
        script: Script.PlutusV3,
        timeout: Duration = 120.seconds
    ): Either[String, (String, Int, TransactionOutput)] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
            val output = TransactionOutput.Babbage(
              address = destinationAddress,
              value = Value.lovelace(50_000_000), // 50 ADA
              datumOption = None,
              scriptRef = Some(ScriptRef(script))
            )

            val tx = TxBuilder(provider.cardanoInfo)
                .output(output)
                .complete(provider, sponsorAddress)
                .await(timeout)
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

    /** Build and submit initialization transaction.
      *
      * Mints the oracle NFT (one-shot policy) and sends it to the script address with the initial
      * ChainState as inline datum. The one-shot UTxO (from params.oneShotTxOutRef) must be
      * consumed.
      */
    def buildAndSubmitInitTransaction(
        signer: TransactionSigner,
        provider: BlockchainProvider,
        scriptAddress: Address,
        sponsorAddress: Address,
        initialState: ChainState,
        script: Script.PlutusV3,
        oneShotUtxo: Utxo,
        lovelaceAmount: Long = 5_000_000L,
        timeout: Duration = 120.seconds
    ): Either[String, String] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
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

            val tx = TxBuilder(provider.cardanoInfo)
                .spend(oneShotUtxo) // consume one-shot UTxO for NFT policy
                .mint(script, mintAssets, mintRedeemer)
                .payTo(scriptAddress, nftValue, initialState)
                .complete(provider, sponsorAddress)
                .await(timeout)
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
        script: Script.PlutusV3,
        referenceScriptUtxo: Option[Utxo] = None,
        timeout: Duration = 120.seconds,
        mpfInsertProofs: ScalusList[ScalusList[ProofStep]] = ScalusList.Nil
    ): Either[String, String] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
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

            val completedBuilder = builder.complete(provider, sponsorAddress).await(timeout)
            val tx = completedBuilder.sign(signer).transaction

            submitTx(provider, tx, timeout)
        } match {
            case Success(result) => result
            case Failure(ex) =>
                Left(
                  s"Error building UpdateOracle transaction: ${ex.getMessage}\n${ex.getStackTrace.take(5).mkString("\n")}"
                )
        }
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

            val completedBuilder = builder.complete(provider, sponsorAddress).await(timeout)
            val tx = completedBuilder.sign(signer).transaction

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
}
