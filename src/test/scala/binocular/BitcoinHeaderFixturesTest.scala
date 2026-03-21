package binocular

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString.hex

/** Tests for BitcoinHeaderFixtures infrastructure
  *
  * Verifies fixture loading, header conversion, and state creation.
  */
class BitcoinHeaderFixturesTest extends AnyFunSuite {

    test("Can load fixture from JSON") {
        val fixture = BitcoinHeaderFixtures.loadFixture("bitcoin-headers-small-3")

        assert(fixture.name == "bitcoin-headers-small-3")
        assert(fixture.startHeight == 865493)
        assert(fixture.endHeight == 865495)
        assert(fixture.headers.length == 3)
    }

    test("Can convert fixture headers to BlockHeader") {
        val headers = BitcoinHeaderFixtures.loadHeaders("bitcoin-headers-small-3")

        assert(headers.length == 3)

        // Verify first header
        val firstHeader = headers.head
        assert(firstHeader.bytes.length == 80, "Block header should be 80 bytes")
    }

    test("Can create genesis state from fixture") {
        val fixture = BitcoinHeaderFixtures.loadFixture("bitcoin-headers-small-3")
        val genesisState = BitcoinHeaderFixtures.createGenesisState(fixture)

        // Verify genesis state properties
        assert(genesisState.ctx.height == BigInt(865493))
        assert(genesisState.ctx.lastBlockHash.length == 32, "Block hash should be 32 bytes")
        assert(genesisState.ctx.currentBits.length == 4, "Compact bits should be 4 bytes")
        assert(!genesisState.ctx.timestamps.isEmpty, "Should have at least one timestamp")
        assert(genesisState.ctx.timestamps.head == BigInt(1736701001))
    }

    test("Fixture headers are sequential") {
        val fixture = BitcoinHeaderFixtures.loadFixture("bitcoin-headers-small-3")

        // Verify heights are consecutive
        val heights = fixture.headers.map(_.height)
        assert(heights == List(865493, 865494, 865495))

        // Verify each header's prevHash matches previous header's hash
        val headers = fixture.headers
        for i <- 1 until headers.length do {
            val prevHeader = headers(i - 1)
            val currentHeader = headers(i)

            assert(
              currentHeader.prevHash == prevHeader.hash,
              s"Block ${currentHeader.height} prevHash should match block ${prevHeader.height} hash"
            )
        }
    }

    test("RPC bits are converted to internal little-endian compact format") {
        val bits = BitcoinChainState.rpcBitsToCompactBits("17030ecd")
        assert(bits == hex"17030ecd".reverse)
    }
}
