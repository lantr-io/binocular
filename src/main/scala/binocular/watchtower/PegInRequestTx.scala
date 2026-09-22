package binocular.watchtower

import scalus.cardano.ledger.{AssetName, Transaction, TransactionInput, Utxo, ValidityInterval, Value}
import scalus.cardano.node.BlockchainProvider
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.txbuilder.TxBuilder
import scalus.cardano.wallet.hd.HdAccount

import scala.concurrent.{ExecutionContext, Future}

/** Builds the permissionless `PegInRequest` mint transaction for one BTC peg-in (Phase C).
  *
  * Mirrors `peg_in.ak`'s mint handler requirements:
  *   - spend `inputRefUtxo` — the one-shot wallet UTxO that becomes `PegInMintRedeemer.input_ref`
  *     (proves freshness; its hash fixes the NFT asset name).
  *   - `oracleUtxo` as a read-only reference input so the script can read `confirmed_blocks_root`.
  *   - mint exactly one NFT named `hash_output_ref(input_ref)` (the 32-byte sha2_256 digest) under
  *     the peg_in policy, with the peg_in script attached as the minting witness.
  *   - send that NFT to the peg_in script address with `datum` (= `PegInRequest.expected_datum`) as
  *     the inline datum.
  *   - set a FINITE validity upper bound at `validToSlot`. `peg_in.ak` pins the datum's `created`
  *     to that bound and traps on an open-ended range ([CLR-7]), so the caller must derive
  *     `expected_datum.created` from the same slot with the same slot config.
  */
object PegInRequestTx {

    def build(
        provider: BlockchainProvider,
        sponsor: HdAccount,
        pegInContract: PegInContract,
        oracleUtxo: Utxo,
        inputRefUtxo: Utxo,
        request: PegInRequest,
        validToSlot: Long,
        lovelaceAmount: Long = 5_000_000L,
        excludeInputs: Set[TransactionInput] = Set.empty
    )(using ExecutionContext): Future[Transaction] = {
        val network = provider.cardanoInfo.network
        val signer = sponsor.signerForUtxos
        val sponsorAddress = sponsor.baseAddress(network)
        require(
          !excludeInputs(inputRefUtxo.input) && inputRefUtxo.output.scriptRef.isEmpty,
          "The one-shot input must not be a reference script"
        )

        val inputRef =
            TxOutRef(TxId(inputRefUtxo.input.transactionId), inputRefUtxo.input.index)
        val assetName = AssetName(PegInContract.assetName(inputRef))
        val redeemer = PegInMintRedeemer(inputRef, request)

        val nftValue =
            Value.lovelace(lovelaceAmount) + Value.asset(pegInContract.policyId, assetName, 1L)

        // The output datum MUST equal the redeemer's expected_datum (peg_in.ak checks
        // peg_in_output.datum == InlineDatum(expected_datum)) — derive it, never pass it separately.
        val builder = TxBuilder(provider.cardanoInfo)
            .validDuring(
              ValidityInterval(invalidBefore = None, invalidHereafter = Some(validToSlot))
            )
            .spend(inputRefUtxo)
            .references(oracleUtxo)
            .mint(pegInContract.script, Map(assetName -> 1L), redeemer)
            .payTo(pegInContract.address(network), nftValue, request.expectedDatum)
        CardanoFunding
            .complete(builder, provider, sponsorAddress, excludeInputs)
            .map(_.sign(signer).transaction)
    }
}
