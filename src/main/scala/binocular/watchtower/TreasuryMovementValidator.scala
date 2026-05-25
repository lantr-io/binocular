package binocular.watchtower

import binocular.*
import binocular.bitcoin.*
import binocular.oracle.{BlockHeader, ChainState}

import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as MPF
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.ProofStep
import scalus.cardano.onchain.plutus.prelude.{List as ScalusList, *}
import scalus.cardano.onchain.plutus.v1.Credential
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.compiler.{Compile, Options}
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.{FromData, ToData, toData}

/** A single fulfilled peg-out parsed from a Treasury Movement output: the raw Bitcoin
  * `scriptPubKey` the TM pays to, and the satoshi `amount`. Mirrors the doc's `fulfilled_peg_outs`
  * entries (technical_documentation.md §"Confirm TM tx").
  */
case class PegOutEntry(scriptPubKey: ByteString, amount: BigInt) derives FromData, ToData

@Compile
object PegOutEntry

/** Datum of the treasury-movement (TM) UTxO.
  *
  *   - [[Unconfirmed]] — created when the signed Bitcoin TM is posted to Cardano. Carries the full
  *     segwit-serialized `signedBtcTx` (the bytes watchtowers relay to Bitcoin). Constr tag 0. This
  *     is the single-field shape heimdall's `publish.rs` (`Constr(0, [signed_btc_tx])`) and
  *     binocular's `create-tmtx` scaffold already post — do not add fields.
  *   - [[Confirmed]] — produced by the Confirm transition once the TM is Binocular-confirmed. Holds
  *     the `btcTxid`, the list of swept peg-in outpoints (`sweptPegInUtxoIds`, 36-byte
  *     prev_txid++vout each), and the `fulfilledPegOuts`. Constr tag 1.
  *
  * Variant order is positional in the Plutus Constr — do not reorder.
  */
enum TmDatum derives FromData, ToData {
    case Unconfirmed(signedBtcTx: ByteString)
    case Confirmed(
        btcTxid: ByteString,
        sweptPegInUtxoIds: ScalusList[ByteString],
        fulfilledPegOuts: ScalusList[PegOutEntry]
    )
}

@Compile
object TmDatum

/** Redeemer for the Confirm transition.
  *
  * @param txIndex
  *   0-based index of the TM tx within its Bitcoin block.
  * @param txMerkleProof
  *   sibling hashes from the TM txid up to the block header's tx-merkle-root.
  * @param blockMpfProof
  *   MPF membership proof that the block hash is in the oracle's `confirmedBlocksRoot`.
  * @param blockHeader
  *   the 80-byte Bitcoin block header (its merkle-root is checked, and it must hash to the
  *   oracle-confirmed block hash).
  */
case class TmConfirmRedeemer(
    txIndex: BigInt,
    txMerkleProof: ScalusList[ByteString],
    blockMpfProof: ScalusList[ProofStep],
    blockHeader: BlockHeader
) derives FromData,
      ToData

@Compile
object TmConfirmRedeemer

/** Treasury-movement validator: enforces the `Unconfirmed -> Confirmed` transition on-chain.
  *
  * This replaces the always-ok scaffold (`TmtxScript`). The only legal spend of an [[TmDatum.Unconfirmed]]
  * UTxO recreates it as [[TmDatum.Confirmed]], and only if the spender *proves* the TM is confirmed
  * on Bitcoin against the Binocular oracle:
  *
  *   1. `txid = sha256d(strip_witness(signedBtcTx))` — recomputed on-chain, never trusted.
  *   2. the block header is in the oracle's `confirmedBlocksRoot` (MPF membership; oracle UTxO is a
  *      reference input, identified by the script hash applied as a compile parameter).
  *   3. the header hashes to the MPF-proven block hash.
  *   4. `txid` is merkle-included in the header's tx-merkle-root at `txIndex`.
  *   5. the continuing output sits at the same TM script address, preserves the UTxO value (so the
  *      TM identity token rides along), and carries a `Confirmed` datum whose `btcTxid` /
  *      `sweptPegInUtxoIds` / `fulfilledPegOuts` are exactly what the contract parsed out of the raw
  *      TM transaction.
  *
  * Parameterized by the Binocular oracle script hash (applied via [[TreasuryMovementContract.contract]]).
  */
@Compile
object TreasuryMovementValidator {

    /** All input outpoints (prev_txid(32) ++ prev_vout(4), 36 bytes each) of a raw Bitcoin tx, in
      * input order. These are the `sweptPegInUtxoIds` of a TM (the old treasury input is included —
      * inert, as no PegInRequest can match it).
      */
    def allInputOutpoints(rawTx: ByteString): ScalusList[ByteString] = {
        val txInsStart = if BitcoinHelpers.isWitnessTransaction(rawTx) then BigInt(6) else BigInt(4)
        val numAndOffset = BitcoinHelpers.readVarInt(rawTx, txInsStart)
        def loop(remaining: BigInt, offset: BigInt): ScalusList[ByteString] =
            if remaining == BigInt(0) then ScalusList.Nil
            else
                val outpoint = rawTx.slice(offset, 36)
                val lenAndAfter = BitcoinHelpers.readVarInt(rawTx, offset + 36)
                val scriptLen = lenAndAfter._1
                val afterVarInt = lenAndAfter._2
                val nextOffset = afterVarInt + scriptLen + 4 // + 4-byte sequence
                ScalusList.Cons(outpoint, loop(remaining - 1, nextOffset))
        loop(numAndOffset._1, numAndOffset._2)
    }

    /** All outputs of a raw Bitcoin tx as `(scriptPubKey, amount)`, in output order. These are the
      * `fulfilledPegOuts` of a TM (the new treasury output is included — inert).
      */
    def allOutputs(rawTx: ByteString): ScalusList[PegOutEntry] = {
        val txInsStart = if BitcoinHelpers.isWitnessTransaction(rawTx) then BigInt(6) else BigInt(4)
        val afterIns = BitcoinHelpers.skipTxIns(rawTx, txInsStart)
        val numAndOffset = BitcoinHelpers.readVarInt(rawTx, afterIns)
        def loop(remaining: BigInt, offset: BigInt): ScalusList[PegOutEntry] =
            if remaining == BigInt(0) then ScalusList.Nil
            else
                val amount = byteStringToInteger(false, rawTx.slice(offset, 8))
                val lenAndAfter = BitcoinHelpers.readVarInt(rawTx, offset + 8)
                val scriptLen = lenAndAfter._1
                val afterVarInt = lenAndAfter._2
                val script = rawTx.slice(afterVarInt, scriptLen)
                val nextOffset = afterVarInt + scriptLen
                ScalusList.Cons(PegOutEntry(script, amount), loop(remaining - 1, nextOffset))
        loop(numAndOffset._1, numAndOffset._2)
    }

    /** Find the Binocular oracle UTxO among reference inputs. Matches both the oracle script hash
      * AND the oracle NFT (policy = oracle script hash, empty asset name, qty 1) so a stray/junk
      * UTxO sitting at the oracle address cannot feed a stale or forged ChainState.
      */
    def findOracleInput(
        refInputs: ScalusList[TxInInfo],
        oracleScriptHash: ByteString
    ): TxOut = {
        def search(remaining: ScalusList[TxInInfo]): TxOut =
            remaining match
                case ScalusList.Nil => fail("Oracle reference input not found")
                case ScalusList.Cons(input, tail) =>
                    val resolved = input.resolved
                    resolved.address.credential match
                        case Credential.ScriptCredential(hash) =>
                            if hash == oracleScriptHash
                                && resolved.value.quantityOf(oracleScriptHash, ByteString.empty) == BigInt(1)
                            then resolved
                            else search(tail)
                        case _ => search(tail)
        search(refInputs)
    }

    /** Resolve the TM script address from the input being spent (identified by `ownRef`). */
    def ownResolved(inputs: ScalusList[TxInInfo], ownRef: TxOutRef): TxOut = {
        def search(remaining: ScalusList[TxInInfo]): TxOut =
            remaining match
                case ScalusList.Nil => fail("Own input not found")
                case ScalusList.Cons(input, tail) =>
                    if equalsData(input.outRef.toData, ownRef.toData) then input.resolved
                    else search(tail)
        search(inputs)
    }

    /** The UNIQUE transaction output paid to `address` (the continuing TM UTxO). Fails if there are
      * zero or more than one — otherwise a spend could add a second TM-address output to split the
      * marker token or seed a parallel forged UTxO while only the first is value/datum-checked.
      */
    def continuingOutput(outputs: ScalusList[TxOut], address: Address): TxOut = {
        def loop(remaining: ScalusList[TxOut], acc: Option[TxOut]): TxOut =
            remaining match
                case ScalusList.Nil =>
                    acc match
                        case Option.Some(o) => o
                        case Option.None    => fail("No continuing TM output")
                case ScalusList.Cons(out, tail) =>
                    if equalsData(out.address.toData, address.toData) then
                        acc match
                            case Option.Some(_) => fail("Multiple TM-address outputs")
                            case Option.None    => loop(tail, Option.Some(out))
                    else loop(tail, acc)
        loop(outputs, Option.None)
    }

    def spend(
        oracleScriptHash: ByteString,
        datumOpt: Option[Datum],
        tx: TxInfo,
        ownRef: TxOutRef,
        redeemer: Datum
    ): Unit = {
        val datum = datumOpt match
            case Option.Some(d) => d.to[TmDatum]
            case Option.None    => fail("Missing TM datum")

        // Only the Unconfirmed -> Confirmed transition is a legal spend.
        val signedBtcTx = datum match
            case TmDatum.Unconfirmed(rawTx) => rawTx
            case _                          => fail("TM UTxO is not Unconfirmed")

        val proof = redeemer.to[TmConfirmRedeemer]

        // 1. Recompute the txid from the witness-stripped serialization — never trust the caller.
        val txid = BitcoinHelpers.getTxHash(signedBtcTx)

        // 2. The block is in the oracle's confirmed-blocks trie.
        val oracleState = findOracleInput(tx.referenceInputs, oracleScriptHash).datum match
            case OutputDatum.OutputDatum(d) => d.to[ChainState]
            case _                          => fail("Oracle must have an inline datum")
        val blockHash = BitcoinHelpers.blockHeaderHash(proof.blockHeader)
        MPF(oracleState.confirmedBlocksRoot).verifyMembership(blockHash, blockHash, proof.blockMpfProof)

        // 3+4. The header hashes to that block hash and commits to txid at txIndex.
        val computedRoot = BitcoinHelpers.merkleRootFromInclusionProof(
          proof.txMerkleProof,
          txid,
          proof.txIndex
        )
        require(computedRoot == proof.blockHeader.merkleRoot, "TM tx not in block merkle root")

        // 5. The continuing output preserves value (TM token carried) and carries the parsed
        //    Confirmed datum.
        val ownOut = ownResolved(tx.inputs, ownRef)
        val contOut = continuingOutput(tx.outputs, ownOut.address)
        require(
          equalsData(contOut.value.toData, ownOut.value.toData),
          "TM value/token not preserved"
        )

        val swept = allInputOutpoints(signedBtcTx)
        val fulfilled = allOutputs(signedBtcTx)
        val expected: Data = (TmDatum.Confirmed(txid, swept, fulfilled): TmDatum).toData
        val actual = contOut.datum match
            case OutputDatum.OutputDatum(d) => d
            case _                          => fail("Confirmed TM output needs an inline datum")
        require(equalsData(actual, expected), "Confirmed datum does not match parsed TM")
    }

    /** Entry point: parse the ScriptContext (spending purpose only) and run [[spend]]. */
    def validate(oracleScriptHash: ByteString, scData: Data): Unit = {
        val sc = unConstrData(scData).snd
        val txInfo = sc.head.to[TxInfo]
        val redeemer = sc.tail.head
        val scriptInfo = unConstrData(sc.tail.tail.head)
        if scriptInfo.fst == BigInt(1) then
            val ownRef = scriptInfo.snd.head.to[TxOutRef]
            val datumOpt = scriptInfo.snd.tail.head.to[Option[Datum]]
            spend(oracleScriptHash, datumOpt, txInfo, ownRef, redeemer)
        else fail("TM validator: not a spending script")
    }
}

/** The TM validator parameterized with the Binocular oracle script hash. The compiled script's hash
  * is the TM UTxO address; `Unconfirmed` UTxOs are locked here and spent into `Confirmed` ones.
  */
object TreasuryMovementContract {
    given opts: Options = Options.release

    /** Curried form: `oracleScriptHash -> (scriptContext -> ())`. Applied via `.apply`, exactly like
      * the always-ok scaffold bakes in its salt.
      */
    lazy val parameterized: PlutusV3[ByteString => (Data => Unit)] =
        PlutusV3.compile((oracleScriptHash: ByteString) =>
            (scData: Data) => TreasuryMovementValidator.validate(oracleScriptHash, scData)
        )

    def contract(oracleScriptHash: ByteString): PlutusV3[Data => Unit] =
        parameterized.apply(oracleScriptHash)
}
