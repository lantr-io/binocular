package binocular

import binocular.bitcoin.{BitcoinHelpers, BlockInfo, RawTransactionInfo, TransactionInfo, VoutInfo}
import binocular.oracle.{reverse, BlockHeader, MerkleTree}
import binocular.watchtower.*

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{AssetName, Credential, LedgerToPlutusTranslation, ScriptHash, TransactionHash, TransactionInput, TransactionOutput, Utxo, Value}
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as MPF
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.crypto.trie.MerklePatriciaForestry as OffChainMPF
import scalus.uplc.builtin.{Builtins, ByteString, Data}
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.{fromData, toData}

import scala.util.Try

/** Unit tests for the [OB-12] deposit-inclusion bundle.
  *
  * The bundle's four items are the `PegInRequest` mint redeemer, so each accepting test verifies
  * them exactly the way `peg_in.ak`'s mint handler does: the header hashes into the oracle's
  * `confirmed_blocks_root` MPF, the tx merkle proof reproduces the header's merkle root, and the
  * raw tx hashes to the proven txid.
  *
  * The deposit fixture is byte-identical to `bifrost/bitcoin.ak`'s `sample_deposit_tx`: vout 0 =
  * P2TR worth 100000 sat, vout 1 = the one-key BFR beacon (`6a 23 "BFR" ‖ Q_auth(32)`). The retired
  * dual-key beacon (`6a 43 "BFR" ‖ D(32) ‖ Q_auth(32)`) is REFUSED on-chain by `beacon_payload`, so
  * the bundle producer must refuse it too — a bundle for it would fail `deposit_binding_ok` at
  * submission.
  */
class PegInProofBundleTest extends AnyFunSuite {

    // bifrost/bitcoin.ak::sample_deposit_tx, verbatim. Non-witness, two outputs.
    private val depositTxHex =
        "020000000100000000000000000000000000000000000000000000000000000000000000000000000000ffffffff02a086010000000000225120bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb0000000000000000256a23424652cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00000000"

    // bifrost/bitcoin.ak::legacy_deposit_tx: the retired dual-key beacon (6a 43 "BFR" ‖ D ‖ Q_auth).
    private val legacyDepositTxHex =
        "020000000100000000000000000000000000000000000000000000000000000000000000000000000000ffffffff02a086010000000000225120bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb0000000000000000456a43424652ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00000000"

    // The key the one-key beacon carries, and the one the retired form put SECOND. `retiredD` only
    // ever appeared in that retired form; no current beacon can carry it.
    private val authKey = "cc" * 32
    private val retiredD = "dd" * 32

    private def voutsOf(rawHex: String): Seq[VoutInfo] = {
        val raw = ByteString.fromHex(rawHex)
        binocular.watchtower.TreasuryMovementValidator
            .allOutputs(raw)
            .asScala
            .toSeq
            .zipWithIndex
            .map { case (out, i) => VoutInfo(i, out.scriptPubKey.toHex, 0.0) }
    }

    /** A single-tx block for `rawHex`: the merkle root IS the txid. */
    private case class Fixture(rawHex: String) {
        val raw: ByteString = ByteString.fromHex(rawHex)
        val txidLE: ByteString = BitcoinHelpers.getTxHash(raw)
        val txidDisplay: String = txidLE.reverse.toHex
        val headerHex: String =
            "00000000" + ("00" * 32) + txidLE.toHex + ("00" * 12)
        val blockHashLE: ByteString =
            BitcoinHelpers.blockHeaderHash(BlockHeader(ByteString.fromHex(headerHex)))
        val blockHashDisplay: String = blockHashLE.reverse.toHex
        val rawTxInfo: RawTransactionInfo =
            RawTransactionInfo(txidDisplay, txidDisplay, rawHex, Some(blockHashDisplay), 10)
        val block: BlockInfo = BlockInfo(
          hash = blockHashDisplay,
          height = 100,
          version = 0,
          merkleroot = txidDisplay,
          time = 0L,
          nonce = 0L,
          bits = "",
          difficulty = 0.0,
          previousblockhash = None,
          tx = Seq(TransactionInfo(txidDisplay, rawHex, voutsOf(rawHex)))
        )
        val mpf: OffChainMPF = OffChainMPF.empty.insert(blockHashLE, blockHashLE)

        def assemble(
            requestedVout: Option[Int],
            mpfOverride: OffChainMPF = mpf
        ): Either[PegInProofBundle.ProduceError, PegInProofBundle] =
            PegInProofBundle.assemble(rawTxInfo, block, headerHex, mpfOverride, requestedVout)
    }

    private val deposit = Fixture(depositTxHex)

    // --- the four [OB-12] items, verified the way peg_in.ak's mint handler does ----------------

    test("the bundle's four items satisfy the mint handler's checks") {
        val bundle = deposit.assemble(requestedVout = Some(0)).toOption.get

        // 1. The raw deposit tx: hashes to the txid the block merkle root commits to.
        assert(BitcoinHelpers.getTxHash(bundle.rawTxHex) == deposit.txidLE)

        // 2. The 80-byte block header.
        assert(bundle.blockHeader.size == 80)

        // 3. Tx merkle proof + index: reproduces the header's merkle root (bytes [36, 68)).
        val headerMerkleRoot = bundle.blockHeader.slice(36, 32)
        val calculated = MerkleTree.calculateMerkleRootFromProof(
          bundle.txIndex,
          deposit.txidLE,
          bundle.txInBlockMerklePath.toList
        )
        assert(calculated == headerMerkleRoot)

        // 4. MPF membership of the block hash against the oracle's confirmed_blocks_root, exactly
        //    as the mint handler verifies it: mpf.has(root, block_hash, block_hash, proof).
        val blockHash = BitcoinHelpers.blockHeaderHash(BlockHeader(bundle.blockHeader))
        assert(MPF(deposit.mpf.rootHash).has(blockHash, blockHash, bundle.mpfHeaderInclusionProof))
    }

    test("the convenience fields mirror deposit_binding_ok's reading of the deposit") {
        val bundle = deposit.assemble(requestedVout = Some(0)).toOption.get
        assert(bundle.pegInVout == 0)
        assert(bundle.pegInAmountSat == 100_000L)
        // The beacon's single key IS the user_source_chain_pub_key. Reading it from the wrong
        // offset would yield the retired form's leading D, so pin that it does not.
        assert(bundle.userSourceChainPubKey == ByteString.fromHex(authKey))
        assert(bundle.userSourceChainPubKey != ByteString.fromHex(retiredD))
        assert(
          bundle.pegInUtxoId == deposit.txidLE ++ hex"00000000"
        )
    }

    test("auto-detection (txid-keyed path) finds the same deposit vout") {
        val bundle = deposit.assemble(requestedVout = None).toOption.get
        assert(bundle.pegInVout == 0)
        assert(bundle.pegInAmountSat == 100_000L)
    }

    // --- outpoint-keyed refusals ---------------------------------------------------------------

    test("an outpoint naming the OP_RETURN vout is refused — it is not the deposit output") {
        assert(
          deposit.assemble(requestedVout = Some(1)) ==
              Left(PegInProofBundle.VoutNotDeposit(deposit.txidDisplay, 1))
        )
    }

    test("an outpoint naming a vout the tx does not have is refused") {
        assert(
          deposit.assemble(requestedVout = Some(7)) ==
              Left(PegInProofBundle.VoutNotDeposit(deposit.txidDisplay, 7))
        )
    }

    test("the retired dual-key beacon is refused, exactly like on-chain beacon_payload") {
        val legacy = Fixture(legacyDepositTxHex)
        assert(
          legacy.assemble(requestedVout = Some(0)) ==
              Left(PegInProofBundle.NoBfrOpReturn(legacy.txidDisplay))
        )
    }

    test("a beacon anywhere but vout 1 is refused — on-chain reads vout 1 only") {
        // Swap the two outputs: P2TR at vout 1, beacon at vout 0.
        val fx = Fixture(depositTxHex)
        val swapped = fx.block.copy(tx =
            Seq(
              TransactionInfo(
                fx.txidDisplay,
                depositTxHex,
                voutsOf(depositTxHex).reverse.zipWithIndex.map { case (v, i) => v.copy(index = i) }
              )
            )
        )
        val res =
            PegInProofBundle.assemble(fx.rawTxInfo, swapped, fx.headerHex, fx.mpf, Some(1))
        assert(res == Left(PegInProofBundle.NoBfrOpReturn(fx.txidDisplay)))
    }

    test("a block the oracle has not confirmed yet is a structured error, not a crash") {
        assert(
          deposit.assemble(requestedVout = Some(0), mpfOverride = OffChainMPF.empty) ==
              Left(
                PegInProofBundle.BlockNotConfirmedByOracle(
                  deposit.txidDisplay,
                  deposit.blockHashDisplay
                )
              )
        )
    }

    // --- completion of the same deposit: [CPI-3] message and [CPI-9] redeemer ------------------
    //
    // The bundle above mints the `PegInRequest`. Completing it is the next step, and rev 5.4 moves
    // both of its inputs off the `Confirmed` TM record ([OB-5]): the signed message drops
    // `tm_txid` ([CPI-3] REVISED), and the sweep is proven against the bridge state singleton's
    // `spi_root` ([CPI-9], [CPI-10]).

    /** The deposit outpoint the depositor completes — the same one the bundle above proves. */
    private val pegInUtxoId: ByteString = deposit.txidLE ++ hex"00000000"

    /** The sweeping TM's input-0 outpoint: the SPI trie value for `pegInUtxoId` ([SPI-3]). */
    private val sweepingTmInput0: ByteString = ByteString.fromHex("77" * 32) ++ hex"00000000"

    /** A txid-shaped value that MUST NOT appear in the [CPI-3] preimage any more. */
    private val tmTxidLE: ByteString = ByteString.fromHex("5a" * 32)

    /** The depositor's chosen fBTC destination, in the Plutus `Address` form the redeemer carries
      * and the signed message commits to (`serialiseData(recipient)`).
      */
    private val recipientData: Data =
        LedgerToPlutusTranslation
            .getAddress(
              Address(Network.Testnet, Credential.ScriptHash(ScriptHash.fromHex("ab" * 28)))
            )
            .toData

    test(
      "the [CPI-3] digest is sha2_256(mint_tag ++ peg_in_utxo_id ++ recipient) and contains no tm_txid"
    ) {
        // The one helper `pegin-complete` and `sign-pegin-msg` share ([OB-11]). This test pins the
        // FORMULA it computes, so the two commands cannot drift apart or re-derive it inline.
        val digest = BifrostMessages.completionDigest(pegInUtxoId, recipientData)

        // [CPI-3] REVISED: mint_tag ‖ peg_in_utxo_id ‖ serialiseData(recipient). Nothing else.
        val expected = Builtins.sha2_256(
          BifrostMessages.mintTag ++ pegInUtxoId ++ Builtins.serialiseData(recipientData)
        )
        assert(
          digest == expected,
          s"[CPI-3] digest ${digest.toHex} != sha2_256(mint_tag ‖ peg_in_utxo_id ‖ " +
              s"serialiseData(recipient)) = ${expected.toHex}"
        )
        assert(digest.size == 32, s"the digest must be 32 bytes, got ${digest.size}")

        // The retired preimage put btc_txid between the tag and the outpoint. No reader can supply
        // it any more (the SPI trie value is the head outpoint), so it MUST be gone.
        val withTmTxid = Builtins.sha2_256(
          BifrostMessages.mintTag ++ tmTxidLE ++ pegInUtxoId ++
              Builtins.serialiseData(recipientData)
        )
        assert(
          digest != withTmTxid,
          "the digest still matches the old preimage that inserts tm_txid after the mint tag"
        )
    }

    test(
      "peg-in completion redeemer carries bridge_state_ref_input_index and the [CPI-9] membership proof"
    ) {
        // The proof and the proven value come from the proof server ([OB-10], [OB-11]).
        val spiTrie = OffChainMPF.empty
            .insert(pegInUtxoId, sweepingTmInput0)
            .insert(ByteString.fromHex("b2" * 32) ++ hex"01000000", sweepingTmInput0)
        val spi = SweptPegInsProofService
            .proveFrom(spiTrie, pegInUtxoId)
            .fold(
              err => fail(s"the SPI proof server refused a swept deposit: ${err.message}"),
              x => x
            )
        assert(spi.sweepingTmInput0 == sweepingTmInput0)
        assert(
          MPF(spiTrie.rootHash).has(pegInUtxoId, spi.sweepingTmInput0, spi.proof),
          "fixture check: the served proof must verify against the trie's own root"
        )

        // The CPI trie insert/exclusion proof, unchanged from rev 5.1.
        val cpiProof: PList[ProofStep] =
            OffChainMPF.empty
                .insert(ByteString.fromHex("c1" * 32) ++ hex"00000000", hex"01")
                .proveNonMembership(pegInUtxoId)

        // `ActionType.CompletePegIn` as `onchain/lib/bifrost/types/peg-in.ak` now defines it:
        // Constr 1 with TEN positional fields, the last three added by rev 5.4 ([CPI-9],
        // [CPI-10]). Field order is consensus-visible, so the mirror must match it exactly.
        val expected: Data = Data.Constr(
          1,
          PList.from(
            List[Data](
              recipientData,
              Data.I(BigInt(0)), // fbtc_output_index
              Data.B(ByteString.fromHex("9a" * 64)), // depositor_signature
              Data.I(BigInt(1)), // completed_peg_in_utxos_input_index
              Data.I(BigInt(2)), // completed_peg_in_utxos_output_index
              cpiProof.toData, // added_peg_in_to_completed_peg_ins_inclusion_proof
              cpiProof.toData, // peg_in_in_completed_peg_ins_exclusion_proof
              Data.I(BigInt(3)), // bridge_state_ref_input_index          [CPI-10]
              Data.B(spi.sweepingTmInput0), // sweeping_tm_input_0        [CPI-9]
              spi.proof.toData // peg_in_swept_membership_proof           [CPI-9]
            )
          )
        )

        val decoded = Try(fromData[PegInActionType](expected))
        assert(
          decoded.isSuccess,
          "the Scalus PegInActionType mirror does not decode the rev-5.4 CompletePegIn redeemer " +
              s"(bridge_state_ref_input_index, sweeping_tm_input_0, [CPI-9] proof): " +
              decoded.failed.map(_.toString).getOrElse("")
        )
        assert(
          decoded.get.toData == expected,
          "the CompletePegIn redeemer does not round-trip: the mirror still carries the fields " +
              s"of the Confirmed-record shape. Got ${decoded.get.toData}"
        )
    }

    test("the PegInDatum mirror decodes the rev-5.4 seven-field datum ending in created") {
        // `onchain/lib/bifrost/types/peg-in.ak` PegInDatum, field for field: owner_auth,
        // source_chain_peg_in_raw_tx, source_chain_peg_in_raw_tx_index, peg_in_utxo_id,
        // peg_in_amount, user_source_chain_pub_key, created. `source_chain_treasury_utxo_id` is
        // gone and `created` ([CLR-7]) is appended, so peg_in_amount moved from position 5 to
        // position 4 and is an Int where the rev-5.1 mirror expects a ByteString.
        val onChainDatum: Data = Data.Constr(
          0,
          PList.from(
            List[Data](
              Data.Constr(0, PList.from(List[Data](Data.B(hex"")))), // owner_auth
              Data.B(deposit.raw), // source_chain_peg_in_raw_tx
              Data.I(BigInt(0)), // source_chain_peg_in_raw_tx_index
              Data.B(pegInUtxoId), // peg_in_utxo_id
              Data.I(BigInt(100000)), // peg_in_amount
              Data.B(ByteString.fromHex("cc" * 32)), // user_source_chain_pub_key
              Data.I(BigInt(1754000000000L)) // created                        [CLR-7]
            )
          )
        )

        val decoded = Try(fromData[PegInDatum](onChainDatum))
        assert(
          decoded.isSuccess,
          "the Scalus PegInDatum mirror does not decode the rev-5.4 datum — pegin-complete " +
              "cannot read any real PegInRequest: " +
              decoded.failed.map(_.toString).getOrElse("")
        )
        assert(
          decoded.get.toData == onChainDatum,
          s"the PegInDatum mirror does not round-trip. Got ${decoded.get.toData}"
        )
        assert(
          decoded.get.pegInAmount == BigInt(100000),
          "peg_in_amount must be read from field 4 — the amount pegin-complete mints"
        )
    }

    test("the completed-peg-ins insert records sweeping_tm_input_0 as the value, not the key") {
        // `peg-in.ak` CompletePegIn step 5 checks
        //   mpf.insert(input_tree, peg_in_utxo_id, sweeping_tm_input_0, proof) == output_tree
        // (spec §The two deposit tries). The CPI trie therefore holds the SAME value the SPI trie
        // holds, so every prior completion's value must be recovered from the SPI trie too.
        val priorId = ByteString.fromHex("b2" * 32) ++ hex"01000000"
        val priorInput0 = ByteString.fromHex("99" * 32) ++ hex"02000000"
        val spiTrie = OffChainMPF.empty
            .insert(pegInUtxoId, sweepingTmInput0)
            .insert(priorId, priorInput0)

        val update = PegInCompleteTx
            .completedPegInsUpdate(Seq(priorId), spiTrie, pegInUtxoId)
            .fold(err => fail(s"the CPI update refused a swept deposit: $err"), x => x)

        val expectedInput = OffChainMPF.empty.insert(priorId, priorInput0)
        assert(
          update.tree.rootHash == expectedInput.rootHash,
          "the reconstructed input trie must record each prior completion under its own " +
              "sweeping_tm_input_0, not under its key"
        )
        val expectedNew = expectedInput.insert(pegInUtxoId, sweepingTmInput0).rootHash
        assert(
          update.newRoot == expectedNew,
          s"the new completed-peg-ins root ${update.newRoot.toHex} != ${expectedNew.toHex} — " +
              "the insert must carry sweeping_tm_input_0 as the value"
        )
        val keyAsValue = expectedInput.insert(pegInUtxoId, pegInUtxoId).rootHash
        assert(
          update.newRoot != keyAsValue,
          "the new root still matches the retired insert-key-as-value shape, which " +
              "added_completed_peg_ins_to_merkle_tree rejects on-chain"
        )
        // Exactly peg-in.ak step 5, on-chain side: insert the deposit into the INPUT trie with the
        // served proof and land on the root the output datum carries.
        assert(
          MPF(update.tree.rootHash)
              .insert(pegInUtxoId, sweepingTmInput0, update.insertProof) == MPF(update.newRoot),
          "the served proof does not carry the input trie to the new root under an insert of " +
              "(peg_in_utxo_id -> sweeping_tm_input_0)"
        )
    }

    test("the completion burns the PegInRequest NFT the spent request carries ([CPI-8])") {
        // peg-in.ak CompletePegIn step 0: the spent PIR must carry exactly one token under the
        // peg_in policy, and `quantity_of(self.mint, peg_in_policy_id, pir_nft_asset_name) == -1`.
        val pegInPolicy = ScriptHash.fromHex("1a" * 28)
        val nftName = AssetName(ByteString.fromHex("2b" * 32))
        val pirAddress = Address(Network.Testnet, Credential.ScriptHash(pegInPolicy))
        val pir = Utxo(
          TransactionInput(TransactionHash.fromHex("3c" * 32), 0),
          TransactionOutput(
            pirAddress,
            Value.lovelace(5_000_000L) + Value.asset(pegInPolicy, nftName, 1L)
          )
        )

        assert(
          PegInCompleteTx.pirNftBurn(pir, pegInPolicy) == Right(nftName -> -1L),
          "the completion must burn exactly the PegInRequest NFT the spent request carries"
        )
        // A request with no NFT under the peg_in policy is not an authentic PIR — refuse rather
        // than build a tx that fails phase 2.
        val bare = Utxo(pir.input, TransactionOutput(pirAddress, Value.lovelace(5_000_000L)))
        assert(
          PegInCompleteTx.pirNftBurn(bare, pegInPolicy).isLeft,
          "a PIR without exactly one peg_in-policy token must be refused"
        )
    }
}
