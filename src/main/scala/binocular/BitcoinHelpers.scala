package binocular

import scalus.uplc.builtin.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString.*
import scalus.cardano.onchain.plutus.prelude.{List, Math, *}
import scalus.cardano.onchain.plutus.prelude.Math.pow
import scalus.*

import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.compiler.Compile

import scala.annotation.tailrec

case class CoinbaseTx(
    version: ByteString,
    inputScriptSigAndSequence: ByteString,
    txOutsAndLockTime: ByteString
) derives FromData,
      ToData

@Compile
object CoinbaseTx

@Compile
object BitcoinHelpers {

    // ============================================================================
    // Bitcoin consensus constants - matches Bitcoin Core chainparams.cpp
    // ============================================================================
    val UnixEpoch: BigInt = 1231006505
    val TargetBlockTime: BigInt = 600 // 10 minutes - matches nPowTargetSpacing in chainparams.cpp
    val DifficultyAdjustmentInterval: BigInt =
        2016 // matches DifficultyAdjustmentInterval() in chainparams.cpp
    val MaxFutureBlockTime: BigInt =
        7200 // 2 hours in the future in seconds - matches MAX_FUTURE_BLOCK_TIME in validation.cpp
    val MedianTimeSpan: BigInt = 11 // matches CBlockIndex::nMedianTimeSpan in chain.h:276
    // Proof of work limit - matches consensus.powLimit in chainparams.cpp
    val PowLimit: BigInt =
        byteStringToInteger(
          true,
          hex"00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
        )

    // Regtest proof of work limit - matches consensus.powLimit in chainparams.cpp (CRegTestParams)
    val RegtestPowLimit: BigInt =
        byteStringToInteger(
          true,
          hex"7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
        )

    // 2^256 constant for chainwork calculation
    // Used in GetBlockProof() formula: work = 2^256 / (target + 1)
    // Precomputed to avoid on-chain exponentiation
    val TwoTo256: BigInt = BigInt(
      "115792089237316195423570985008687907853269984665640564039457584007913129639936"
    )

    /** Calculate proof-of-work for a block given its target Matches GetBlockProof() in Bitcoin
      * Core's chain.cpp
      *
      * Formula: 2^256^ / (target + 1)
      *
      * This represents the expected number of hashes needed to find a valid block at this
      * difficulty
      */
    def calculateBlockProof(target: BigInt): BigInt = {
        TwoTo256 / (target + 1)
    }

    // Double SHA256 hash - matches CBlockHeader::GetHash() in primitives/block.h
    def blockHeaderHash(blockHeader: BlockHeader): BlockHash =
        sha2_256(sha2_256(blockHeader.bytes))

    /** Converts little-endian compact bits representation to target value
      *
      * Matches `arith_uint256::SetCompact()` in arith_uint256.cpp
      * @note
      *   only works for positive numbers. Here we use it only for `target` which is always
      *   positive, so it's fine.
      */
    def compactBitsToTarget(compact: CompactBits): BigInt = {
        // int nSize = nCompact >> 24;
        val exponent = compact.at(3)
        // uint32_t nWord = nCompact & 0x007fffff;
        val coefficient = byteStringToInteger(false, compact.slice(0, 3))
        if coefficient > BigInt(0x007fffff) then fail("Negative bits")
        else if exponent < BigInt(3)
        then coefficient / Math.pow(256, BigInt(3) - exponent)
        // check overflow
        else if coefficient != BigInt(0) && ((exponent > 34) ||
                (coefficient > 0xff && exponent > 33) ||
                (coefficient > 0xffff && exponent > 32))
        then fail("Bits overflow")
        else coefficient * pow(256, exponent - 3)
    }

    /** Converts target value (256 bits BigInt) to a little-endian compact 32 bits representation
      *
      * Matches `arith_uint256::GetCompact()` in arith_uint256.cpp
      * @note
      *   only works for positive numbers. Here we use it only for `target` which is always
      *   positive, so it's fine.
      */
    def targetToCompactBits(target: BigInt): BigInt = {
        if target == BigInt(0) then 0
        else
            // Calculate the number of significant bytes by finding the highest non-zero byte
            // Convert to 32-byte little-endian representation to find significant bytes easily
            val targetBytes = integerToByteString(false, 32, target)

            // Find the number of significant bytes (from most significant end)
            @tailrec
            def findSignificantBytes(bytes: ByteString, index: BigInt): BigInt = {
                if index < 0 then BigInt(0)
                else if bytes.at(index) != BigInt(0) then index + 1
                else findSignificantBytes(bytes, index - 1)
            }

            val nSize = findSignificantBytes(targetBytes, BigInt(31))

            // Extract nCompact according to Bitcoin Core GetCompact logic
            val nCompact =
                if nSize <= 3 then
                    // nCompact = GetLow64() << 8 * (3 - nSize)
                    target * pow(256, 3 - nSize)
                // For large values: extract the most significant 3 bytes
                else target / pow(BigInt(256), nSize - 3)

            val (finalSize, finalCompact) =
                if nCompact >= BigInt(0x800000) then (nSize + 1, nCompact / BigInt(256))
                else (nSize, nCompact)
            // Pack into compact representation: [3 bytes mantissa][1 byte size]
            finalCompact + (finalSize * BigInt(0x1000000)) // size << 24
    }

    /** Converts target value to compact bits using findFirstSetBit builtin.
      *
      * Same as [[targetToCompactBits]] but replaces the recursive findSignificantBytes loop with a
      * single findFirstSetBit builtin call. On a little-endian 32-byte representation,
      * findFirstSetBit scans from the most-significant end, so `32 - result / 8` gives nSize.
      */
    def targetToCompactBitsV2(target: BigInt): BigInt = {
        if target == BigInt(0) then 0
        else
            val targetBytes = integerToByteString(false, 32, target)
            val nSize = BigInt(32) - findFirstSetBit(targetBytes) / 8

            val nCompact =
                if nSize <= 3 then target * pow(256, 3 - nSize)
                else target / pow(BigInt(256), nSize - 3)

            val (finalSize, finalCompact) =
                if nCompact >= BigInt(0x800000) then (nSize + 1, nCompact / BigInt(256))
                else (nSize, nCompact)
            finalCompact + (finalSize * BigInt(0x1000000))
    }

    /** Converts target value to little-endian compact bits representation
      *
      * Matches `arith_uint256::GetCompact()` in arith_uint256.cpp
      * @note
      *   only works for positive numbers. Here we use it only for `target` which is always
      *   positive, so it's fine.
      */
    def targetToCompactByteString(target: BigInt): CompactBits = {
        val compact = targetToCompactBitsV2(target)
        integerToByteString(false, 4, compact)
    }

    /** Gets the median time of past blocks from a reverse-sorted timestamp list.
      *
      * Matches CBlockIndex::GetMedianTimePast() in chain.h:278-290:
      * {{{
      * int64_t GetMedianTimePast() const {
      *     int64_t pmedian[nMedianTimeSpan];
      *     int64_t* pbegin = &pmedian[nMedianTimeSpan];
      *     int64_t* pend = &pmedian[nMedianTimeSpan];
      *     const CBlockIndex* pindex = this;
      *     for (int i = 0; i < nMedianTimeSpan && pindex; i++, pindex = pindex->pprev)
      *         *(--pbegin) = pindex->GetBlockTime();
      *     std::sort(pbegin, pend);
      *     return pbegin[(pend - pbegin) / 2];
      * }
      * }}}
      *
      * Bitcoin Core collects up to 11 timestamps, sorts ascending, and returns the element at index
      * `count / 2`. Here, `timestamps` is maintained in descending (reverse-sorted) order. For an
      * odd-sized list (always 11 in steady state), the element at `size / 2` is the same regardless
      * of sort direction — both yield the true median.
      *
      * @param timestamps
      *   reverse-sorted list of block timestamps (newest/largest first)
      * @param size
      *   number of elements in the list
      * @return
      *   the median timestamp, or [[UnixEpoch]] if the list is empty
      */
    def getMedianTimePast(timestamps: List[BigInt], size: BigInt): BigInt =
        timestamps match
            case List.Nil => UnixEpoch
            case _ =>
                val index = size / 2
                timestamps !! index

    def merkleRootFromInclusionProof(
        merkleProof: List[TxHash],
        hash: TxHash,
        index: BigInt
    ): MerkleRoot =
        def loop(index: BigInt, curHash: ByteString, siblings: List[ByteString]): ByteString =
            if siblings.isEmpty then curHash
            else
                val sibling = siblings.head
                val nextHash =
                    if index % 2 == BigInt(0)
                    then sha2_256(sha2_256(curHash ++ sibling))
                    else sha2_256(sha2_256(sibling ++ curHash))
                loop(index / 2, nextHash, siblings.tail)

        loop(index, hash, merkleProof)

    def readVarInt(input: ByteString, offset: BigInt): (BigInt, BigInt) =
        val firstByte = input.at(offset)
        if firstByte < BigInt(0xfd) then (firstByte, offset + 1)
        else if firstByte == BigInt(0xfd) then
            (byteStringToInteger(false, input.slice(offset + 1, 2)), offset + 3)
        else if firstByte == BigInt(0xfe) then
            (byteStringToInteger(false, input.slice(offset + 1, 4)), offset + 5)
        else (byteStringToInteger(false, input.slice(offset + 1, 8)), offset + 9)

    def parseCoinbaseTxScriptSig(coinbaseTx: ByteString): ByteString =
        // Skip version
        // 4
        // marker and flag of witness transaction
        // + 1 + 1
        // Read input count (should be 1 for coinbase)
        // + 1
        // Skip previous transaction hash and output index
        // + 32 + 4
        val offset = BigInt(43)
        val (scriptLength, newOffset) = readVarInt(coinbaseTx, offset)
        val scriptSig = coinbaseTx.slice(newOffset, scriptLength)
        scriptSig

    def parseBlockHeightFromScriptSig(scriptSig: ByteString): BigInt =
        val len = scriptSig.at(0)
        require(1 <= len && len <= 8, "Invalid block height length")
        val height = byteStringToInteger(false, scriptSig.slice(1, len))
        height

    def getBlockHeightFromCoinbaseTx(tx: CoinbaseTx): BigInt =
        val scriptSigAndSequence = tx.inputScriptSigAndSequence
        val (scriptLength, newOffset) = readVarInt(scriptSigAndSequence, 0)
        // read first byte of scriptSig
        // this MUST be OP_PUSHBYTES_len
        val len = scriptSigAndSequence.at(newOffset)
        require(1 <= len && len <= 8, "Invalid block height length")
        val height = byteStringToInteger(false, scriptSigAndSequence.slice(newOffset + 1, len))
        height

    def getTxHash(rawTx: ByteString): TxHash =
        val serializedTx = stripWitnessData(rawTx)
        sha2_256(sha2_256(serializedTx))

    def isWitnessTransaction(rawTx: ByteString): Boolean =
        rawTx.at(4) == BigInt(0) && rawTx.at(5) == BigInt(1)

    def stripWitnessData(rawTx: ByteString): ByteString =
        if isWitnessTransaction(rawTx) then
            // Format: [nVersion][marker][flag][txins][txouts][witness][nLockTime]
            val version = rawTx.slice(0, 4)
            val txInsStartIndex = BigInt(6) // Skip marker and flag bytes
            val txOutsOffset = skipTxIns(rawTx, txInsStartIndex)
            val outsEnd = skipTxOuts(rawTx, txOutsOffset)
            val txIns = rawTx.slice(txInsStartIndex, txOutsOffset - txInsStartIndex)
            val txOuts = rawTx.slice(txOutsOffset, outsEnd - txOutsOffset)
            val lockTimeOsset = outsEnd + 1 + 1 + 32 // Skip witness data
            val lockTime = rawTx.slice(lockTimeOsset, 4)
            version ++ txIns ++ txOuts ++ lockTime
        else rawTx

    def getCoinbaseTxHash(coinbaseTx: CoinbaseTx): TxHash =
        val serializedTx = coinbaseTx.version
            ++ hex"010000000000000000000000000000000000000000000000000000000000000000ffffffff"
            ++ coinbaseTx.inputScriptSigAndSequence
            ++ coinbaseTx.txOutsAndLockTime
        sha2_256(sha2_256(serializedTx))

    def skipTxIns(rawTx: ByteString, txInsStartIndex: BigInt): BigInt =
        val (numIns, newOffset) = readVarInt(rawTx, txInsStartIndex)
        def loop(num: BigInt, offset: BigInt): BigInt =
            if num == BigInt(0) then offset
            else loop(num - 1, skipTxIn(rawTx, offset))
        loop(numIns, newOffset)

    def skipTxOuts(rawTx: ByteString, offset: BigInt): BigInt =
        val (numOuts, newOffset) = readVarInt(rawTx, offset)
        def loop(numOuts: BigInt, offset: BigInt): BigInt =
            if numOuts == BigInt(0) then offset
            else loop(numOuts - 1, skipTxOut(rawTx, offset))
        loop(numOuts, newOffset)

    def skipTxIn(rawTx: ByteString, offset: BigInt): BigInt =
        // Skip tx hash 32 and index 4
        val (scriptLength, newOffset) = readVarInt(rawTx, offset + 36)
        newOffset + scriptLength + 4 // sequence is 4

    def skipTxOut(rawTx: ByteString, offset: BigInt): BigInt =
        // Skip tx hash 32 and index 4
        val (scriptLength, newOffset) = readVarInt(rawTx, offset + 8) // skip amount
        newOffset + scriptLength

    /** Read the outpoint (prev_txid ++ prev_vout, 36 bytes) of the first input.
      * Handles both witness-serialized (marker+flag at bytes 4-5) and legacy format.
      *
      * @param rawTx Bitcoin transaction bytes (witness or non-witness serialized)
      * @return 36-byte outpoint of the first input
      */
    def firstInputOutpoint(rawTx: ByteString): ByteString =
        val txInsStart = if isWitnessTransaction(rawTx) then BigInt(6) else BigInt(4)
        val (numIns, firstInputOffset) = readVarInt(rawTx, txInsStart)
        require(numIns > 0, "Transaction has no inputs")
        rawTx.slice(firstInputOffset, 36)

    /** Return the 0-based index of the input whose prev_txid matches `targetPrevTxid`.
      * Fails with "TM does not spend peg-in tx" if no matching input is found.
      * Handles both witness-serialized and non-witness tx bytes.
      *
      * @param rawTx Bitcoin transaction bytes
      * @param targetPrevTxid 32-byte txid to find (in internal/LE byte order, i.e. sha256d output)
      * @return 0-based index of the matching input
      */
    def findPegInInputIndex(rawTx: ByteString, targetPrevTxid: TxHash): BigInt =
        val txInsStart = if isWitnessTransaction(rawTx) then BigInt(6) else BigInt(4)
        val (numIns, firstInputOffset) = readVarInt(rawTx, txInsStart)
        def loop(remaining: BigInt, offset: BigInt, index: BigInt): BigInt =
            if remaining == BigInt(0) then fail("TM does not spend peg-in tx")
            else
                val prevTxid = rawTx.slice(offset, 32)
                val (scriptLen, afterVarInt) = readVarInt(rawTx, offset + 36)
                val nextOffset = afterVarInt + scriptLen + 4 // skip sequence (4 bytes)
                if prevTxid == targetPrevTxid then index
                else loop(remaining - 1, nextOffset, index + 1)
        loop(numIns, firstInputOffset, 0)

    /** Return the byte offset where the witness data begins in a witness-serialized tx.
      * The witness section follows immediately after all inputs and all outputs.
      * Fails if the transaction is not witness-serialized.
      *
      * @param rawTx Witness-serialized Bitcoin transaction bytes
      * @return Byte offset of the first byte of the witness section
      */
    def findWitnessSectionOffset(rawTx: ByteString): BigInt =
        require(isWitnessTransaction(rawTx), "Transaction is not witness-serialized")
        val afterInputs = skipTxIns(rawTx, 6) // 6 = version(4) + marker(1) + flag(1)
        skipTxOuts(rawTx, afterInputs)

    // ============================================================================
    // Taproot witness classification (BIP341 / BIP342)
    //
    // Wire format for each input's witness inside a segwit transaction:
    //
    //   [varint: N]                     ← number of stack items
    //   [varint: len][len bytes]         ← item 0
    //   [varint: len][len bytes]         ← item 1
    //   ...                             ← item N-1
    //
    // The witness section contains one such block per transaction input,
    // in input order. An input with no witness data has N = 0 (single 0x00 byte).
    //
    // KEY-PATH SPEND (BIP341 §Script validation rules, key-path)
    // ──────────────────────────────────────────────────────────
    // N = 1, item 0 = 64-byte Schnorr signature (SIGHASH_DEFAULT)
    //                or 65-byte sig with explicit hash type appended.
    //
    //   01          ← N = 1
    //   40          ← item 0 length = 64
    //   <sig 64B>   ← bare Schnorr signature over the tweaked key Q
    //
    // SCRIPT-PATH SPEND (BIP341 §Script validation rules, script-path)
    // ─────────────────────────────────────────────────────────────────
    // N = (script_inputs) + 2.  The last two items are always:
    //   item[N-2]: the leaf script being executed
    //   item[N-1]: the control block
    //
    // Control block layout (BIP341 §Script validation rules):
    //   byte 0     : (leaf_version & 0xfe) | parity_of_output_key_Q
    //                  0xc0 = Tapscript leaf version (BIP342) + even Q
    //                  0xc1 = Tapscript leaf version + odd  Q
    //   bytes 1-32 : internal key x-coordinate (Y_51 in Bifrost)
    //   bytes 33+  : Merkle path — one 32-byte sibling hash per tree level
    //                  depth 0 (single leaf, no siblings): 33 bytes total
    //                  depth 1 (2-leaf tree):              65 bytes total
    //                  depth 2 (4-leaf tree):              97 bytes total
    //
    // BIFROST PROTOCOL SCRIPT LEAVES — always 3-item witnesses
    // ─────────────────────────────────────────────────────────
    // All Bifrost script leaves are single-signature (`<key> OP_CHECKSIG`
    // or `<timeout> OP_CSV OP_DROP <key> OP_CHECKSIG`).  These scripts need
    // exactly one witness stack input (the signature), so the witness is:
    //
    //   item 0: signature            (64 B)
    //   item 1: leaf script          (e.g. 34 B for <32B key> OP_CHECKSIG)
    //   item 2: control block        (65 B for a 2-leaf tree at depth 1)
    //
    // Example — Y_67 script leaf spend on the treasury:
    //
    //   03                            ← N = 3
    //   40  <sig 64B>                 ← Schnorr sig over the TM sighash
    //   22  20 <Y_67 32B> ac          ← script: OP_PUSHBYTES_32 <Y_67> OP_CHECKSIG
    //   41  c0 <Y_51 32B> <hash 32B>  ← control block: leaf_ver=0xc0, internal=Y_51, 1 sibling
    //
    // DEPOSITOR CSV REFUND — always 4-item witness
    // ─────────────────────────────────────────────
    // The depositor refund leaf script:
    //   <4320> OP_CSV OP_DROP OP_DUP OP_HASH160 <hash160(pubkey) 20B> OP_EQUALVERIFY OP_CHECKSIG
    //
    // It hardcodes only the HASH160 of the pubkey, not the pubkey itself.
    // At spend time the depositor must supply the full x-only pubkey as a
    // witness item so the script can verify OP_HASH160 matches.  This adds
    // one extra item compared with a federation spend, giving 4 items total:
    //
    //   04                            ← N = 4
    //   40  <sig 64B>                 ← Schnorr sig
    //   20  <pubkey 32B>              ← depositor x-only pubkey (needed for HASH160 check)
    //   XX  <refund script>           ← leaf script
    //   41  <control block 65B>       ← control block
    //
    // This 3-vs-4 item count is the reliable discriminator between a
    // legitimate federation sweep and a depositor CSV refund.
    // ============================================================================

    /** Skip one witness stack entry and return the offset of the next witness.
      *
      * Each input's witness begins at `offset` with the varint item count, followed by
      * `[varint len][len bytes]` for each item. See wire-format comment block above.
      *
      * @param rawTx Bitcoin transaction bytes
      * @param offset Byte offset of the varint that starts this witness's stack item count
      * @return Byte offset immediately after this witness (= start of the next input's witness)
      */
    def skipOneWitness(rawTx: ByteString, offset: BigInt): BigInt =
        val (stackItems, afterCount) = readVarInt(rawTx, offset)
        def loop(n: BigInt, off: BigInt): BigInt =
            if n == BigInt(0) then off
            else
                val (itemLen, afterLen) = readVarInt(rawTx, off)
                loop(n - 1, afterLen + itemLen)
        loop(stackItems, afterCount)

    /** Return the number of stack items in the witness of input at `inputIndex`.
      *
      * Navigates to the target input's witness by skipping `inputIndex` witnesses
      * (each starting with its own item-count varint), then reads and returns that varint.
      * See wire-format comment block above for encoding details.
      *
      * @param rawTx Witness-serialized Bitcoin transaction bytes
      * @param inputIndex 0-based index of the input whose witness to inspect
      * @return number of stack items (N) in that input's witness
      */
    def witnessStackSize(rawTx: ByteString, inputIndex: BigInt): BigInt =
        val witnessStart = findWitnessSectionOffset(rawTx)
        def skip(n: BigInt, off: BigInt): BigInt =
            if n == BigInt(0) then off
            else skip(n - 1, skipOneWitness(rawTx, off))
        val witnessOff = skip(inputIndex, witnessStart)
        // witnessOff points to the varint N (item count) that opens this input's witness block.
        // readVarInt returns (N, offset_after_N); we only need N here.
        val (stackItems, _) = readVarInt(rawTx, witnessOff)
        stackItems

    /** Return true iff the witness at `inputIndex` is a Taproot key-path spend.
      *
      * A key-path spend has exactly 1 witness item: a bare Schnorr signature.
      *   - 64 bytes: SIGHASH_DEFAULT (implicit, most common)
      *   - 65 bytes: explicit hash type byte appended
      *
      * Any script-path spend has ≥ 2 items (script inputs + leaf_script + control_block),
      * so item count = 1 is a sufficient discriminator.
      *
      * @param rawTx Witness-serialized Bitcoin transaction bytes
      * @param inputIndex 0-based index of the input whose witness to inspect
      * @return true if witness is a key-path spend (N=1, item length 64-65)
      */
    def isKeyPathWitness(rawTx: ByteString, inputIndex: BigInt): Boolean =
        val witnessStart = findWitnessSectionOffset(rawTx)
        def skip(n: BigInt, off: BigInt): BigInt =
            if n == BigInt(0) then off
            else skip(n - 1, skipOneWitness(rawTx, off))
        val witnessOff = skip(inputIndex, witnessStart)
        val (stackItems, afterCount) = readVarInt(rawTx, witnessOff)
        if stackItems != BigInt(1) then false
        else
            val (itemLen, _) = readVarInt(rawTx, afterCount)
            itemLen >= BigInt(64) && itemLen <= BigInt(65)

    /** Return true iff the witness at `inputIndex` is a Bifrost protocol script-path spend.
      *
      * All Bifrost script leaves require exactly 1 signature as witness input
      * (Y_67 OP_CHECKSIG; or timeout OP_CSV OP_DROP Y_fed OP_CHECKSIG), producing
      * exactly 3 witness items:
      *   item 0: signature  (64 B)
      *   item 1: leaf script
      *   item 2: control block  (33 + 32*depth bytes; 65 B for a 2-leaf tree)
      *
      * The depositor CSV refund leaf (`OP_HASH160 <hash> OP_EQUALVERIFY OP_CHECKSIG`)
      * requires the depositor's x-only pubkey as an explicit witness item so the script
      * can verify HASH160(pubkey) == stored_hash.  This gives 4 items, not 3.
      *
      * Count == 3 is therefore a reliable discriminator: it accepts Y_67 and Y_fed
      * script-path spends while rejecting depositor refunds and malformed witnesses.
      *
      * @param rawTx Witness-serialized Bitcoin transaction bytes
      * @param inputIndex 0-based index of the input whose witness to inspect
      * @return true if witness is a 3-item protocol script-path (N=3)
      */
    def isValidScriptPathWitness(rawTx: ByteString, inputIndex: BigInt): Boolean =
        witnessStackSize(rawTx, inputIndex) == BigInt(3)

    // Insert a timestamp into a sorted list while maintaining order
    def insertReverseSorted(value: BigInt, sortedValues: List[BigInt]): List[BigInt] =
        sortedValues match
            case List.Nil => List.single(value)
            case List.Cons(head, tail) =>
                if value >= head then List.Cons(value, sortedValues)
                else List.Cons(head, insertReverseSorted(value, tail))

    // Difficulty adjustment - matches GetNextWorkRequired() in pow.cpp:14-67
    //
    // Bitcoin Core source (paraphrased):
    //   if ((pindexLast->nHeight+1) % DifficultyAdjustmentInterval() != 0) {
    //     if (params.fPowAllowMinDifficultyBlocks) {
    //       if (pblock->GetBlockTime() > pindexLast->GetBlockTime() + nPowTargetSpacing*2)
    //         return nProofOfWorkLimit;
    //       else { /* walk back to last non-min-difficulty block */ return pindex->nBits; }
    //     }
    //     return pindexLast->nBits;
    //   }
    //   return CalculateNextWorkRequired(...);
    //
    // The "walk back" loop reduces to `currentTarget` here because Binocular's
    // `accumulateBlock` never overwrites `ctx.currentBits` between retargets — it
    // already holds the period's last non-min-difficulty value (the bits computed at
    // the most recent retarget boundary). Min-difficulty blocks intentionally do NOT
    // update it.
    //
    // @param newBlockTime the timestamp of the block being validated (only consulted when
    //                     `allowMinDifficultyBlocks` is set)
    // @param prevBlockTime the timestamp of the previous block (`pindexLast->GetBlockTime()`),
    //                      used both as `nActualTimespan`'s upper bound at retargets and as
    //                      the parent timestamp for the testnet 20-minute gap check.
    // @param allowMinDifficultyBlocks Bitcoin Core's `consensus.fPowAllowMinDifficultyBlocks`
    //                                 (testnet3 / testnet4 / regtest only).
    def getNextWorkRequired(
        nHeight: BigInt,
        currentTarget: CompactBits,
        prevBlockTime: BigInt,
        nFirstBlockTime: BigInt,
        powLimit: BigInt,
        newBlockTime: BigInt,
        allowMinDifficultyBlocks: Boolean
    ): CompactBits = {
        // Only change once per difficulty adjustment interval
        if (nHeight + 1) % DifficultyAdjustmentInterval == BigInt(0) then
            val newTarget =
                calculateNextWorkRequired(currentTarget, prevBlockTime, nFirstBlockTime, powLimit)
            targetToCompactByteString(newTarget)
        else if allowMinDifficultyBlocks && newBlockTime > prevBlockTime + 2 * TargetBlockTime
        then
            // testnet/testnet4/regtest 20-minute exception: a block whose timestamp is more
            // than 2 * TargetBlockTime after its parent may use the proof-of-work limit.
            targetToCompactByteString(powLimit)
        else currentTarget
    }

    // Calculate next work required - matches CalculateNextWorkRequired() in pow.cpp:50-84
    def calculateNextWorkRequired(
        currentTarget: CompactBits,
        blockTime: BigInt,
        nFirstBlockTime: BigInt,
        powLimit: BigInt
    ): BigInt = {
        val PowTargetTimespan = DifficultyAdjustmentInterval * TargetBlockTime
        val actualTimespan =
            val timespan = blockTime - nFirstBlockTime
            // Limit adjustment step - matches pow.cpp:55-60
            Math.min(
              Math.max(timespan, PowTargetTimespan / 4),
              PowTargetTimespan * 4
            )

        val t = compactBitsToTarget(currentTarget)
        val bnNew = t * actualTimespan / PowTargetTimespan
        Math.min(bnNew, powLimit)
    }
}
