package binocular

import munit.FunSuite

/** Tests for BitcoinHeaderFixtures infrastructure
  *
  * Verifies fixture loading, header conversion, and state creation.
  */
class BitcoinHeaderFixturesTest extends FunSuite {

    test("Can load fixture from JSON") {
        val fixture = BitcoinHeaderFixtures.loadFixture("bitcoin-headers-small-3")

        assertEquals(fixture.name, "bitcoin-headers-small-3")
        assertEquals(fixture.startHeight, 865493)
        assertEquals(fixture.endHeight, 865495)
        assertEquals(fixture.headers.length, 3)
    }

    test("Can convert fixture headers to BlockHeader") {
        val headers = BitcoinHeaderFixtures.loadHeaders("bitcoin-headers-small-3")

        assertEquals(headers.length, 3)

        // Verify first header
        val firstHeader = headers.head
        assert(firstHeader.bytes.length == 80, "Block header should be 80 bytes")
    }

    test("Can create genesis state from fixture") {
        val fixture = BitcoinHeaderFixtures.loadFixture("bitcoin-headers-small-3")
        val genesisState = BitcoinHeaderFixtures.createGenesisState(fixture)

        // Verify genesis state properties
        assertEquals(genesisState.blockHeight, BigInt(865493))
        assert(genesisState.blockHash.length == 32, "Block hash should be 32 bytes")
        assert(genesisState.currentTarget.length == 4, "Compact bits should be 4 bytes")
        assert(!genesisState.recentTimestamps.isEmpty, "Should have at least one timestamp")
        assertEquals(genesisState.blockTimestamp, BigInt(1736701001))
    }

    test("Fixture headers are sequential") {
        val fixture = BitcoinHeaderFixtures.loadFixture("bitcoin-headers-small-3")

        // Verify heights are consecutive
        val heights = fixture.headers.map(_.height)
        assertEquals(heights, List(865493, 865494, 865495))

        // Verify each header's prevHash matches previous header's hash
        val headers = fixture.headers
        for (i <- 1 until headers.length) {
            val prevHeader = headers(i - 1)
            val currentHeader = headers(i)

            assertEquals(
              currentHeader.prevHash,
              prevHeader.hash,
              s"Block ${currentHeader.height} prevHash should match block ${prevHeader.height} hash"
            )
        }
    }
}
