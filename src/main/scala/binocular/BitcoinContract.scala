package binocular

import scalus.*
import scalus.cardano.blueprint.{Blueprint, HasTypeDescription, Preamble, Validator}
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.v3.TxOutRef
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
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

    // Maximum blocks allowed in the fork tree. 256 = 2^8, the capacity of a balanced
    // binary tree at depth 8 — the most space-efficient tree shape (15,248 bytes), which
    // fits within the 16,384-byte tx size limit with ~1,136 bytes of headroom for the
    // redeemer. This bounds the cheapest griefing attack: filling the tree with single-block
    // branches (all below maturationConfirmations) so nothing qualifies for promotion.
    val DefaultMaxBlocksInForkTree: Int = 256

    /** Build default params from a TxOutRef and owner */
    def validatorParams(
        txOutRef: TxOutRef,
        owner: PubKeyHash,
        maturationConfirmations: Int = 100,
        challengeAging: Int = 12000,
        closureTimeout: Int = 2592000,
        maxBlocksInForkTree: Int = DefaultMaxBlocksInForkTree,
        testingMode: Boolean = false
    ): BitcoinValidatorParams =
        BitcoinValidatorParams(
          maturationConfirmations = maturationConfirmations,
          challengeAging = challengeAging,
          oneShotTxOutRef = txOutRef,
          closureTimeout = closureTimeout,
          owner = owner,
          powLimit = BitcoinHelpers.PowLimit,
          maxBlocksInForkTree = maxBlocksInForkTree,
          testingMode = testingMode
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
              redeemer = Some(summon[HasTypeDescription[OracleAction]].typeDescription),
              datum = Some(summon[HasTypeDescription[ChainState]].typeDescription),
              parameters = Some(scala.List(param)),
              compiledCode = Some(compiled.program.cborEncoded.toHex),
              hash = Some(compiled.script.scriptHash.toHex)
            )
          )
        )
    }
}
