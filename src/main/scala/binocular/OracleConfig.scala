package binocular

import pureconfig.*
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.Credential
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.uplc.builtin.ByteString

import scala.util.{Failure, Success, Try}

/** Configuration for Binocular Oracle.
  *
  * Loaded automatically by PureConfig from reference.conf / application.conf / env vars.
  */
case class OracleConfig(
    txOutRef: String = "",
    ownerPkh: String = "",
    startHeight: Option[Long] = None,
    maxHeadersPerTx: Int = 10,
    pollInterval: Int = 1,
    retryInterval: Int = 10,
    transactionTimeout: Int = 120,
    maturationConfirmations: Int = 100,
    challengeAging: Int = 12000,
    closureTimeout: Int = 2592000,
    testingMode: Boolean = false
) derives ConfigReader {

    /** Parse txOutRef and ownerPkh into BitcoinValidatorParams */
    def toBitcoinValidatorParams(): Either[String, BitcoinValidatorParams] = {
        for {
            ref <- parseTxOutRef(txOutRef)
            pkh <- parseOwnerPkh(ownerPkh)
        } yield BitcoinContract.validatorParams(
          ref,
          pkh,
          maturationConfirmations = maturationConfirmations,
          challengeAging = challengeAging,
          closureTimeout = closureTimeout,
          testingMode = testingMode
        )
    }

    /** Derive script address for a given network */
    def scriptAddress(network: CardanoNetwork): Either[String, String] = {
        toBitcoinValidatorParams().map { params =>
            val scriptHash = BitcoinContract.makeContract(params).script.scriptHash
            val scalusNetwork = network match {
                case CardanoNetwork.Mainnet => Network.Mainnet
                case CardanoNetwork.Preprod => Network.Testnet
                case CardanoNetwork.Preview => Network.Testnet
                case CardanoNetwork.Testnet => Network.Testnet
            }
            val address = Address(scalusNetwork, Credential.ScriptHash(scriptHash))
            address.asInstanceOf[scalus.cardano.address.ShelleyAddress].toBech32.get
        }
    }

    /** Validate configuration */
    def validate(): Either[String, Unit] = {
        if txOutRef.isEmpty then Left("oracle.tx-out-ref must be configured. Set ORACLE_TX_OUT_REF")
        else if ownerPkh.isEmpty then
            Left("oracle.owner-pkh must be configured. Set ORACLE_OWNER_PKH")
        else toBitcoinValidatorParams().map(_ => ())
    }

    private def parseTxOutRef(s: String): Either[String, TxOutRef] = {
        s.split("#") match {
            case Array(txHash, idx) =>
                Try {
                    TxOutRef(TxId(ByteString.fromHex(txHash)), BigInt(idx.toInt))
                } match {
                    case Success(ref) => Right(ref)
                    case Failure(ex)  => Left(s"Invalid TxOutRef format '$s': ${ex.getMessage}")
                }
            case _ => Left(s"Invalid TxOutRef format '$s'. Expected: <txhash>#<index>")
        }
    }

    private def parseOwnerPkh(s: String): Either[String, PubKeyHash] = {
        Try(PubKeyHash(ByteString.fromHex(s))) match {
            case Success(pkh) => Right(pkh)
            case Failure(ex)  => Left(s"Invalid owner PKH '$s': ${ex.getMessage}")
        }
    }

    override def toString: String = {
        s"OracleConfig(txOutRef=$txOutRef, startHeight=$startHeight, maxHeadersPerTx=$maxHeadersPerTx, pollInterval=$pollInterval, retryInterval=$retryInterval, transactionTimeout=$transactionTimeout, maturationConfirmations=$maturationConfirmations, challengeAging=$challengeAging, closureTimeout=$closureTimeout, testingMode=$testingMode)"
    }
}
