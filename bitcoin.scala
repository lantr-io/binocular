package binocular

import scalus.builtin.ByteString
import onchain.*
import scalus.builtin.Builtins.*
import BitcoinValidator.*

object Bitcoin {
    def isWitnessTransaction(rawTx: ByteString): Boolean =
        rawTx.index(4) == BigInt(0) && indexByteString(rawTx, 5) == BigInt(1)

    def makeCoinbaseTxFromByteString(rawTx: ByteString): CoinbaseTx = {
        val version = rawTx.slice(0, 4)
        if isWitnessTransaction(rawTx) then
            val (inputScriptSigAndSequence, txOutsOffset) =
                val scriptSigStart =
                    4 + 2 + 1 + 36 // version [4] + [marker][flag] 2 + txInCount [01] + txhash [32] + txindex [4]
                val (scriptLength, newOffset) = readVarInt(rawTx, scriptSigStart)
                val end = newOffset + scriptLength + 4
                val len = end - scriptSigStart
                (sliceByteString(scriptSigStart, len, rawTx), end)
            val txOutsAndLockTime =
                val outsEnd = skipTxOuts(rawTx, txOutsOffset)
                val lockTimeOffset = outsEnd + 1 + 1 + 32 // Skip witness data
                val txOutsLen = outsEnd - txOutsOffset
                val txOuts = sliceByteString(txOutsOffset, txOutsLen, rawTx)
                val lockTime = sliceByteString(lockTimeOffset, 4, rawTx)
                appendByteString(txOuts, lockTime)
            CoinbaseTx(
              version = version,
              inputScriptSigAndSequence = inputScriptSigAndSequence,
              txOutsAndLockTime
            )
        else
            val (inputScriptSigAndSequence, txOutsOffset) =
                val scriptSigStart =
                    4 + 1 + 36 // version [4] + txInCount [01] + txhash [32] + txindex [4]
                val (scriptLength, newOffset) = readVarInt(rawTx, scriptSigStart)
                val end = newOffset + scriptLength + 4
                val len = end - scriptSigStart
                (sliceByteString(scriptSigStart, len, rawTx), end)
            val txOutsAndLockTime =
                val outsEnd = skipTxOuts(rawTx, txOutsOffset)
                val lockTimeOffset = outsEnd
                val txOutsLen = outsEnd - txOutsOffset
                val len = txOutsLen + 4
                sliceByteString(txOutsOffset, len, rawTx)
            CoinbaseTx(
              version = version,
              inputScriptSigAndSequence = inputScriptSigAndSequence,
              txOutsAndLockTime
            )
    }
}
