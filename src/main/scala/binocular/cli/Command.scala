package binocular.cli

import binocular.ChainState
import com.bloxbean.cardano.client.api.model.Utxo
import scalus.builtin.{ByteString, Data}
import scalus.builtin.Data.fromData
import scalus.prelude.List as ScalusList

import scala.util.Try

/** Base trait for all CLI commands
  *
  * Each command implements this trait and provides its execution logic. This allows for better
  * separation of concerns, easier testing, and cleaner maintainability.
  */
trait Command {

    /** Execute the command
      *
      * @return
      *   Exit code (0 for success, non-zero for error)
      */
    def execute(): Int
}

/** Represents a validated oracle UTxO with its parsed ChainState */
case class ValidOracleUtxo(
    utxo: Utxo,
    chainState: ChainState
) {
    def txHash: String = utxo.getTxHash
    def outputIndex: Int = utxo.getOutputIndex
    def utxoRef: String = s"$txHash:$outputIndex"
}

/** Helper utilities for commands */
object CommandHelpers {

    /** Parse UTxO reference string (TX_HASH:OUTPUT_INDEX)
      *
      * @param utxo
      *   UTxO reference string
      * @return
      *   Either error message or (txHash, outputIndex)
      */
    def parseUtxo(utxo: String): Either[String, (String, Int)] = {
        val parts = utxo.split(":")
        if parts.length != 2 then {
            Left(s"Invalid UTxO format. Expected: <TX_HASH>:<OUTPUT_INDEX>")
        } else {
            parts(1).toIntOption match {
                case Some(index) => Right((parts(0), index))
                case None        => Left(s"Invalid output index: ${parts(1)}")
            }
        }
    }

    /** Print error and exit
      *
      * @param message
      *   Error message
      * @param exitCode
      *   Exit code (default: 1)
      */
    def exitWithError(message: String, exitCode: Int = 1): Nothing = {
        System.err.println(s"Error: $message")
        System.exit(exitCode)
        throw new RuntimeException() // Never reached
    }

    /** Check if a UTxO is an oracle UTxO (has inline datum, no reference script) */
    def isOracleUtxo(utxo: Utxo): Boolean =
        utxo.getInlineDatum != null &&
            utxo.getInlineDatum.nonEmpty &&
            utxo.getReferenceScriptHash == null

    /** Try to parse ChainState from UTxO's inline datum */
    def parseChainState(utxo: Utxo): Option[ChainState] =
        Option(utxo.getInlineDatum).filter(_.nonEmpty).flatMap { datumHex =>
            Try {
                val bs = ByteString.fromHex(datumHex)
                val data = Data.fromCbor(bs)
                fromData[ChainState](data)
            }.toOption
        }

    /** Check if ChainState is valid (has 11 sorted timestamps) */
    def isValidChainState(chainState: ChainState): Boolean = {
        def toScalaList(l: ScalusList[BigInt]): scala.List[BigInt] = l match {
            case ScalusList.Nil        => scala.Nil
            case ScalusList.Cons(h, t) => h :: toScalaList(t)
        }
        val timestamps = toScalaList(chainState.recentTimestamps)
        timestamps.size >= 11 && timestamps.sliding(2).forall {
            case Seq(a, b) => a >= b
            case _         => true
        }
    }

    /** Try to get a valid oracle UTxO from a raw UTxO Returns Some(ValidOracleUtxo) if UTxO has
      * valid datum with valid ChainState
      */
    def tryValidateOracleUtxo(utxo: Utxo): Option[ValidOracleUtxo] =
        if !isOracleUtxo(utxo) then None
        else
            parseChainState(utxo).filter(isValidChainState).map { chainState =>
                ValidOracleUtxo(utxo, chainState)
            }

    /** Filter list of UTxOs to only valid oracle UTxOs */
    def filterValidOracleUtxos(utxos: List[Utxo]): List[ValidOracleUtxo] =
        utxos.flatMap(tryValidateOracleUtxo)
}
