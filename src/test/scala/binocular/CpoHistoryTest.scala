package binocular

import binocular.watchtower.{BlockfrostCpoHistory, ChainOutput}

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.Data

/** Tests for the chain-history backend: failure classification, retries, and the THREE-way reading
  * of an output's datum.
  *
  * All three exist for the same reason. [[binocular.watchtower.PorSweeper]] latches a permanent
  * halt on a failure it cannot explain, so a source that reports a rate-limit the same way it
  * reports a corrupted trie would disable peg-out cleanup for the process's life over an HTTP 429.
  */
class CpoHistoryTest extends AnyFunSuite {

    private val txHash = "aa" * 32

    // --- failure classification -----------------------------------------------------------------

    test("rate limiting and server faults are retryable; client faults are not") {
        assert(BlockfrostCpoHistory.isRetryable(429))
        assert(BlockfrostCpoHistory.isRetryable(408))
        assert(BlockfrostCpoHistory.isRetryable(500))
        assert(BlockfrostCpoHistory.isRetryable(503))
        // A bad project id or a rejected address is a configuration fault; retrying cannot fix it,
        // and retrying a 403 against a hosted API is how a project gets banned.
        assert(!BlockfrostCpoHistory.isRetryable(400))
        assert(!BlockfrostCpoHistory.isRetryable(403))
        assert(!BlockfrostCpoHistory.isRetryable(404))
    }

    test("a transport failure is retried with exponential backoff, then reported as TRANSIENT") {
        // Port 1 on loopback refuses instantly, so this exercises the retry loop with no server and
        // no wall-clock cost (the sleep is injected).
        val slept = scala.collection.mutable.ListBuffer.empty[Long]
        val source = new BlockfrostCpoHistory(
          baseUrl = "http://127.0.0.1:1",
          projectId = "",
          maxAttempts = 4,
          baseBackoffMs = 10L,
          sleep = ms => slept += ms
        )
        val err = source.addressHistory("addr_test").swap.getOrElse(fail("expected a failure"))
        assert(err.transient, "a refused connection says nothing about the chain")
        assert(err.message.contains("gave up after 4 attempts"))
        // One sleep per retry, doubling: 4 attempts = 3 backoffs.
        assert(slept.toList == List(10L, 20L, 40L))
    }

    test("a single attempt does not sleep") {
        val slept = scala.collection.mutable.ListBuffer.empty[Long]
        val source = new BlockfrostCpoHistory(
          baseUrl = "http://127.0.0.1:1",
          projectId = "",
          maxAttempts = 1,
          baseBackoffMs = 10L,
          sleep = ms => slept += ms
        )
        assert(source.addressHistory("addr_test").isLeft)
        assert(slept.isEmpty)
    }

    // --- datum presence -------------------------------------------------------------------------

    private def out(json: String): ChainOutput =
        BlockfrostCpoHistory.parseOutput(txHash, ujson.read(json))

    test("an output with no datum reports NEITHER a datum nor a problem") {
        // The distinction that keeps one junk payment from blocking every reconstruction: a TM
        // record always carries an inline datum, so this provably is not one.
        val o = out(s"""{"output_index":0,"amount":[{"unit":"lovelace","quantity":"2000000"}]}""")
        assert(o.inlineDatum.isEmpty)
        assert(o.unresolvedDatum.isEmpty)
    }

    test("a datum hash with no served preimage is reported as UNRESOLVED") {
        // The output HAS a datum; this backend just cannot read it. It might be a Confirmed record,
        // so reconstruction must stop rather than skip.
        val o = out(s"""{"output_index":1,"data_hash":"${"bb" * 32}","amount":[]}""")
        assert(o.inlineDatum.isEmpty)
        assert(o.unresolvedDatum.exists(_.contains("no resolvable preimage")))
    }

    test("an inline datum that does not decode is reported as UNRESOLVED, not as absent") {
        val o = out("""{"output_index":2,"inline_datum":"zzzz","amount":[]}""")
        assert(o.inlineDatum.isEmpty)
        assert(o.unresolvedDatum.exists(_.contains("did not decode")))
    }

    test("a decodable inline datum is resolved") {
        // CBOR for the integer 1.
        val o = out("""{"output_index":3,"inline_datum":"01","amount":[]}""")
        assert(o.inlineDatum == Some(Data.I(BigInt(1))))
        assert(o.unresolvedDatum.isEmpty)
    }

    test("assets are keyed by Blockfrost unit and read case-insensitively") {
        val o = out(
          """{"output_index":4,"amount":[
             {"unit":"lovelace","quantity":"2000000"},
             {"unit":"A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A166534154","quantity":"100000"}
           ]}"""
        )
        assert(o.quantityOf("a1" * 28, "66534154") == BigInt(100_000))
        assert(o.quantityOf("a1" * 28, "deadbeef") == BigInt(0))
    }
}
