package binocular

import binocular.bitcoin.SimpleBitcoinRpc.satsFromGetTxOut

import org.scalatest.funsuite.AnyFunSuite

/** The BTC-to-satoshi conversion behind `gettxout`.
  *
  * Worth its own suite because of what the number is used for: it becomes the bridge-state
  * singleton's `treasury_amount`, and the first Treasury Movement's BIP-341 sighash commits to it.
  * One satoshi out and every FROST signature the roster produces over that movement is invalid —
  * with nothing on Cardano able to detect it, since the amount is a Bitcoin fact.
  */
class GetTxOutValueTest extends AnyFunSuite {

    private def sats(json: String): Option[Long] = satsFromGetTxOut("aa" * 32, 0, ujson.read(json))

    test("null means spent or never existed, indistinguishably") {
        assert(sats("null").isEmpty)
    }

    test("whole and fractional BTC convert exactly") {
        assert(sats("""{"value": 1}""").contains(100_000_000L))
        assert(sats("""{"value": 0.1}""").contains(10_000_000L))
        assert(sats("""{"value": 0.00000001}""").contains(1L))
        assert(sats("""{"value": 0.12345678}""").contains(12_345_678L))
    }

    /** 0.1 has no exact binary floating-point representation, so a Double route reaches this answer
      * by rounding rather than by construction. Pinning the awkward decimals states that the
      * conversion is exact by construction instead.
      */
    test("decimals that are not representable as doubles still convert exactly") {
        for (btc, expected) <- List(
              ("0.1", 10_000_000L),
              ("0.2", 20_000_000L),
              ("0.3", 30_000_000L),
              ("0.07", 7_000_000L),
              ("2.675", 267_500_000L)
            )
        do assert(sats(s"""{"value": $btc}""").contains(expected), s"$btc BTC")
    }

    /** Near the 21M cap the Double's ulp is a sizeable fraction of a satoshi. */
    test("amounts near the supply cap convert exactly") {
        assert(sats("""{"value": 20999999.9769}""").contains(2_099_999_997_690_000L))
        assert(sats("""{"value": 21000000}""").contains(2_100_000_000_000_000L))
    }

    /** Some RPC proxies serialize `value` as a JSON string; `ujson`'s `.num` throws on those. */
    test("a value returned as a JSON string is accepted") {
        assert(sats("""{"value": "0.1"}""").contains(10_000_000L))
    }

    /** Sub-satoshi precision is not a Bitcoin amount, so it is a mis-encode — better to fail than
      * to silently round the deployment anchor's value.
      */
    test("a sub-satoshi value is refused rather than rounded") {
        val e = intercept[IllegalStateException](sats("""{"value": 0.000000001}"""))
        assert(e.getMessage.contains("whole number of satoshi"), e.getMessage)
    }

    test("a non-numeric value is refused") {
        val e = intercept[IllegalStateException](sats("""{"value": "not-a-number"}"""))
        assert(e.getMessage.contains("non-numeric"), e.getMessage)
    }
}
