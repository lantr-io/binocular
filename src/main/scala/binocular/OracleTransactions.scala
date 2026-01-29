package binocular

import binocular.util.SlotConfigHelper
import scalus.cardano.address.Address
import scalus.cardano.ledger.{CardanoInfo, DatumOption, PlutusScript, Script, ScriptRef, Transaction, TransactionHash, TransactionInput, TransactionOutput, Utxo, Utxos, Value}
import scalus.cardano.node.{BlockchainProvider, SubmitError, UtxoQuery, UtxoSource}
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.prelude.List as ScalusList

import java.time.Instant
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

/** Helper functions for building oracle transactions */
object OracleTransactions {

    /** Get compiled PlutusV3 script */
    def getPlutusScript(): Script.PlutusV3 = {
        Script.PlutusV3(BitcoinContract.bitcoinProgram.cborByteString)
    }

    /** Deploy the oracle script to a UTxO as a reference script. This allows subsequent
      * transactions to use the script without including it in the tx body, significantly reducing
      * transaction size.
      *
      * @param signer
      *   TransactionSigner to sign the transaction
      * @param provider
      *   BlockchainProvider for querying and submitting
      * @param sponsorAddress
      *   Address to pay fees from
      * @param destinationAddress
      *   Address to deploy reference script to
      * @param timeout
      *   Transaction timeout
      * @return
      *   Either error message or (txHash, outputIndex, savedOutput) of the reference script UTxO.
      *   The savedOutput is the TransactionOutput we constructed, which includes scriptRef. Use it
      *   to build the Utxo locally after tx confirmation, since chain queries may not return the
      *   scriptRef field.
      */
    def deployReferenceScript(
        signer: TransactionSigner,
        provider: BlockchainProvider,
        sponsorAddress: Address,
        destinationAddress: Address,
        timeout: Duration = 120.seconds
    ): Either[String, (String, Int, TransactionOutput)] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
            val script = getPlutusScript()

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

    /** Find existing reference script UTxOs at a given address
      *
      * Searches for UTxOs that contain the oracle script as a reference script.
      *
      * @param provider
      *   BlockchainProvider
      * @param searchAddress
      *   Address to search for reference scripts
      * @param timeout
      *   Query timeout
      * @return
      *   List of (txHash, outputIndex) tuples for UTxOs with matching reference script.
      */
    def findReferenceScriptUtxos(
        provider: BlockchainProvider,
        searchAddress: Address,
        timeout: Duration = 30.seconds
    ): List[(String, Int)] = {
        given ec: ExecutionContext = provider.executionContext
        val script = getPlutusScript()
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

    /** Compute the validity interval start time that will be used on-chain.
      *
      * @param cardanoInfo
      *   CardanoInfo from BlockchainProvider
      * @return
      *   (validityInstant, timeInSeconds)
      */
    def computeValidityIntervalTime(cardanoInfo: CardanoInfo): (Instant, BigInt) = {
        SlotConfigHelper.computeValidityIntervalTime(cardanoInfo)
    }

    /** Apply Bitcoin headers to ChainState to calculate new state */
    def applyHeaders(
        currentState: ChainState,
        headers: ScalusList[BlockHeader],
        currentTime: BigInt
    ): ChainState = {
        headers.foldLeft(currentState) { (state, header) =>
            BitcoinValidator.updateTip(state, header, currentTime)
        }
    }

    /** Build and submit initialization transaction
      *
      * Creates a new oracle UTxO with initial ChainState datum.
      *
      * @param signer
      *   TransactionSigner for signing the transaction
      * @param provider
      *   BlockchainProvider for querying and submitting
      * @param scriptAddress
      *   Oracle script address
      * @param sponsorAddress
      *   Address to pay fees from
      * @param initialState
      *   Initial ChainState datum
      * @param lovelaceAmount
      *   Amount of ADA to lock (default: 5 ADA)
      * @param timeout
      *   Transaction timeout
      * @return
      *   Either error message or transaction hash
      */
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

    /** Build and submit UpdateOracle transaction
      *
      * Updates the oracle with new Bitcoin block headers.
      *
      * @param signer
      *   TransactionSigner for signing the transaction
      * @param provider
      *   BlockchainProvider for querying and submitting
      * @param scriptAddress
      *   Oracle script address
      * @param sponsorAddress
      *   Address to pay fees from
      * @param oracleUtxo
      *   Current oracle UTxO (input, output)
      * @param currentChainState
      *   Current ChainState datum
      * @param newChainState
      *   New ChainState datum (pre-computed)
      * @param blockHeaders
      *   Bitcoin block headers to submit
      * @param validityIntervalTimeSeconds
      *   The time (in seconds) used to compute newChainState
      * @param referenceScriptUtxo
      *   Optional reference script UTxO
      * @param timeout
      *   Transaction timeout
      * @return
      *   Either error message or transaction hash
      */
    def buildAndSubmitUpdateTransaction(
        signer: TransactionSigner,
        provider: BlockchainProvider,
        scriptAddress: Address,
        sponsorAddress: Address,
        oracleUtxo: Utxo,
        currentChainState: ChainState,
        newChainState: ChainState,
        blockHeaders: ScalusList[BlockHeader],
        validityIntervalTimeSeconds: BigInt,
        referenceScriptUtxo: Option[Utxo] = None,
        timeout: Duration = 120.seconds
    ): Either[String, String] = {
        given ec: ExecutionContext = provider.executionContext
        Try {
            val script = getPlutusScript()

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
            val (validityInstant, validatorWillSeeTime) = computeValidityIntervalTime(cardanoInfo)

            println(s"[DEBUG] Computing time that validator will see:")
            println(s"  validatorWillSeeTime (seconds): $validatorWillSeeTime")

            // Verify time tolerance
            val TimeToleranceSeconds = BitcoinValidator.TimeToleranceSeconds.toLong
            val timeDiff =
                if validityIntervalTimeSeconds > validatorWillSeeTime then
                    validityIntervalTimeSeconds - validatorWillSeeTime
                else validatorWillSeeTime - validityIntervalTimeSeconds

            println(
              s"[DEBUG] Provided time: $validityIntervalTimeSeconds, Validator will see: $validatorWillSeeTime"
            )
            println(s"  Time difference: $timeDiff seconds")

            if timeDiff > TimeToleranceSeconds then {
                throw new RuntimeException(
                  s"Time mismatch: provided time ($validityIntervalTimeSeconds) differs from tx.validRange time ($validatorWillSeeTime) by $timeDiff seconds (tolerance: $TimeToleranceSeconds s)."
                )
            }
            println(s"  Time difference within tolerance - using provided time for redeemer")

            val redeemerTime = validityIntervalTimeSeconds

            // Compute input datum hash for redeemer
            val inputDatumHash = scalus.uplc.builtin.Builtins.blake2b_256(
              scalus.uplc.builtin.Builtins.serialiseData(currentChainState.toData)
            )

            // Create UpdateOracle action as redeemer
            val action = Action.UpdateOracle(blockHeaders, redeemerTime, inputDatumHash)

            println(s"[DEBUG] New state forksTree size: ${newChainState.forksTree.size}")

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
                // Script resolved from references
                builder.spend(oracleUtxo, action)
            } else {
                // Attach script directly
                builder.spend(oracleUtxo, action, script)
            }

            // Output new state with same value
            builder = builder
                .payTo(scriptAddress, oracleUtxo.output.value, newChainState)
                .validFrom(validityInstant)

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
