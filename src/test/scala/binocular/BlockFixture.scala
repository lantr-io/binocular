package binocular

import binocular.BitcoinHelpers.*
import scalus.uplc.builtin.ByteString
import upickle.default.*

import java.nio.file.Files

/** Block fixture loaded from JSON test data in src/test/resources/bitcoin_blocks/ */
case class BlockFixture(
    height: Int,
    hash: String,
    merkleroot: String,
    transactions: Seq[String] = Seq.empty,
    previousblockhash: Option[String] = None,
    timestamp: Long,
    bits: String,
    nonce: Long,
    version: Long,
    difficulty: Option[Double] = None,
    description: Option[String] = None
) derives ReadWriter

object BlockFixture {
    private val defaultFixtureDir = "src/test/resources/bitcoin_blocks"

    private def longToLE4Bytes(n: Long): ByteString = ByteString.fromArray(
      Array(
        (n & 0xff).toByte,
        ((n >> 8) & 0xff).toByte,
        ((n >> 16) & 0xff).toByte,
        ((n >> 24) & 0xff).toByte
      )
    )

    /** Load a block fixture from JSON by height */
    def load(height: Int, fixtureDir: String = defaultFixtureDir): BlockFixture = {
        load(new java.io.File(s"$fixtureDir/block_$height.json"))
    }

    /** Load a block fixture from a JSON file */
    def load(file: java.io.File): BlockFixture = {
        val json = Files.readString(file.toPath)
        read[BlockFixture](json)
    }

    /** Convert a BlockFixture to an 80-byte BlockHeader */
    def toBlockHeader(fixture: BlockFixture): BlockHeader = {
        BlockHeader(
          longToLE4Bytes(fixture.version) ++
              ByteString.fromHex(fixture.previousblockhash.getOrElse(
                "0000000000000000000000000000000000000000000000000000000000000000"
              )).reverse ++
              ByteString.fromHex(fixture.merkleroot).reverse ++
              longToLE4Bytes(fixture.timestamp) ++
              ByteString.fromHex(fixture.bits).reverse ++
              longToLE4Bytes(fixture.nonce)
        )
    }

    /** Load a block fixture and return both the fixture and its BlockHeader */
    def loadWithHeader(
        height: Int,
        fixtureDir: String = defaultFixtureDir
    ): (BlockFixture, BlockHeader) = {
        val fixture = load(height, fixtureDir)
        val header = toBlockHeader(fixture)

        // Verify hash matches
        val computedHash = blockHeaderHash(header)
        assert(
          computedHash.reverse.toHex == fixture.hash,
          s"Hash mismatch for block ${fixture.height}: computed=${computedHash.reverse.toHex}, expected=${fixture.hash}"
        )

        (fixture, header)
    }
}
