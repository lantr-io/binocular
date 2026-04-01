package binocular

import scalus.uplc.builtin.Builtins.sha2_256
import scalus.uplc.builtin.ByteString

import scala.collection.Seq
import scala.collection.mutable.{ArrayBuffer, ArraySeq}

/** Rolling Merkle tree implementation
  */
class MerkleTreeRootBuilder {
    private val levels: ArrayBuffer[ByteString] = ArrayBuffer.empty

    private def addHashAtLevel(hash: ByteString, startingLevel: Int): this.type = {
        var levelHash = hash
        var level = startingLevel

        // Process each level to find a place for the new hash
        while level < levels.length do
            if levels(level) == null then
                // If no hash is present at this level, just add the current hash
                levels(level) = levelHash
                return this
            else
                // If there is a hash, combine it with the current hash and move up one level
                levelHash = sha2_256(sha2_256(levels(level) ++ levelHash))
                levels(level) = null // Clear the hash as it's been moved up
                level += 1

        // If we exit the loop, it means we're adding a new level to the tree
        levels.append(levelHash)
        this
    }

    def addHash(hash: ByteString): this.type = {
        addHashAtLevel(hash, 0)
        this
    }

    def getMerkleRoot: ByteString =
        if levels.isEmpty then ByteString.unsafeFromArray(new Array[Byte](32))
        else if levels.size == 1 then levels.head
        else
            var index = 0
            while index < levels.length do
                if levels(index) != null && index < levels.length - 1
                then addHashAtLevel(levels(index), index)
                index += 1
            levels.last
}

class MerkleTree(private val levels: collection.IndexedSeq[collection.IndexedSeq[ByteString]]) {
    assert(levels.nonEmpty)

    def getMerkleRoot: ByteString = {
        levels.last.head
    }

    def makeMerkleProof(index: Int): Seq[ByteString] = {
        val proofSize = levels.length - 1
        val hashesCount = levels.head.length
        assert(index < hashesCount)
        if proofSize == 0 then return ArraySeq.empty

        val proof = ArraySeq.newBuilder[ByteString]
        for level <- 0 until proofSize do
            val levelHashes = levels(level)
            val idx = index / (1 << level)
            val proofHashIdx = if idx % 2 == 0 then idx + 1 else idx - 1
            proof += levelHashes(proofHashIdx)

        proof.result()
    }

    override def toString: String =
        levels.map(_.map(_.toHex.take(8)).mkString(",")).mkString("\n")
}

object MerkleTree {
    def fromHashes(hashes: collection.Seq[ByteString]): MerkleTree = {
        @annotation.tailrec
        def buildLevels(
            currentLevelHashes: ArrayBuffer[ByteString],
            accumulatedLevels: ArrayBuffer[ArrayBuffer[ByteString]]
        ): ArrayBuffer[ArrayBuffer[ByteString]] = {
            if currentLevelHashes.length == 1 then
                // If only one hash is left, add it to the levels and finish
                accumulatedLevels += currentLevelHashes
                accumulatedLevels
            else
                // Calculate the next level and recurse
                val nextLevelHashes = calculateMerkleTreeLevel(currentLevelHashes)
                accumulatedLevels += currentLevelHashes
                buildLevels(nextLevelHashes, accumulatedLevels)
        }

        if hashes.isEmpty then
            MerkleTree(ArrayBuffer(ArrayBuffer(ByteString.unsafeFromArray(new Array[Byte](32)))))
        else if hashes.length == 1 then MerkleTree(ArrayBuffer(ArrayBuffer.from(hashes)))
        else MerkleTree(buildLevels(ArrayBuffer.from(hashes), ArrayBuffer.empty))
    }

    private def calculateMerkleTreeLevel(
        hashes: ArrayBuffer[ByteString]
    ): ArrayBuffer[ByteString] = {
        val levelHashes = ArrayBuffer.empty[ByteString]
        // Duplicate the last element if the number of elements is odd
        if hashes.length % 2 == 1
        then hashes.addOne(hashes.last)

        for i <- hashes.indices by 2 do
            val combinedHash = hashes(i) ++ hashes(i + 1)
            levelHashes += sha2_256(sha2_256(combinedHash))
        levelHashes
    }

    def calculateMerkleRootFromProof(
        index: Int,
        hash: ByteString,
        proof: Seq[ByteString]
    ): ByteString = {
        var idx = index
        var currentHash = hash

        for sibling <- proof do
            val combinedHash =
                if idx % 2 == 0 then currentHash ++ sibling
                else sibling ++ currentHash
            currentHash = sha2_256(sha2_256(combinedHash))
            idx /= 2
        currentHash
    }
}
