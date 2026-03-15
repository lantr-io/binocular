package binocular

import scalus.*
import scalus.cardano.blueprint.{Blueprint, HasTypeDescription, Preamble, Validator}
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.cardano.onchain.plutus.v3.TxOutRef
import scalus.compiler.Options
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.uplc.PlutusV3
import scalus.utils.Hex.toHex

object BitcoinContract {
    given opts: Options = Options.release.copy(
      generateErrorTraces = false,
      targetProtocolVersion = MajorProtocolVersion.plominPV
    )

    lazy val contract: PlutusV3[Data => Data => Unit] =
        PlutusV3.compile(BitcoinValidator.validate)

    def makeContract(params: BitcoinValidatorParams): PlutusV3[Data => Unit] =
        contract(params.toData)

    /** Build default params from a TxOutRef */
    def validatorParams(txOutRef: TxOutRef): BitcoinValidatorParams =
        BitcoinValidatorParams(
          maturationConfirmations = 100,
          challengeAging = 200 * 60,
          oneShotTxOutRef = txOutRef
        )

    lazy val blueprint: Blueprint = {
        val title = "Binocular – a trustless Bitcoin Oracle"
        val description =
            """Binocular is a Bitcoin oracle for Cardano that enables smart contracts to verify Bitcoin blockchain
              |state. Anyone can submit Bitcoin block headers to a single on-chain Oracle without registration or
              |bonding. The Cardano smart contract validates all blocks against Bitcoin consensus rules (
              |proof-of-work, difficulty, timestamps) and automatically selects the canonical chain. Blocks with
              |100+ confirmations and 200+ minutes on-chain aging are promoted to confirmed state, enabling
              |transaction inclusion proofs. Security relies on a 1-honest-party assumption and Bitcoin's
              |proof-of-work, with attack costs exceeding $46 million.""".stripMargin
        val compiled = contract
        val param = summon[HasTypeDescription[BitcoinValidatorParams]].typeDescription
        Blueprint(
          preamble = Preamble(
            title,
            description,
            "1.0.0",
            plutusVersion = compiled.language,
            license = Some("Apache-2.0")
          ),
          validators = Seq(
            Validator(
              title = title,
              description = Some(description),
              redeemer = Some(summon[HasTypeDescription[UpdateOracle]].typeDescription),
              datum = Some(summon[HasTypeDescription[ChainState]].typeDescription),
              parameters = Some(scala.List(param)),
              compiledCode = Some(compiled.program.cborEncoded.toHex),
              hash = Some(compiled.script.scriptHash.toHex)
            )
          )
        )
    }
}
