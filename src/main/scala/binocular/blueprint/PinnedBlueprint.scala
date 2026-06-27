package binocular.blueprint

import scalus.cardano.ledger.Script
import scalus.uplc.builtin.{Builtins, ByteString, Data}

/** Frozen, fully-applied script blueprint (Strategy A).
  *
  * `PlutusV3` applies parameters at the SIR level and re-lowers/re-optimizes, so a Scalus script
  * hash cannot be reconstructed from un-applied UPLC by applying params at load time (unlike Aiken's
  * `compiledCode`, which [[binocular.watchtower.BifrostBlueprint]] applies via `Program.$`).
  * Therefore we freeze each validator's *fully-applied* bytes and load them verbatim — no compile,
  * no optimize, no param application at load — which is trivially correct and immune to compiler
  * upgrades.
  *
  * Parameterized validators have multiple live deployments (e.g. preprod vs testnet4 oracle params
  * differ), so applied entries are keyed by [[PinnedBlueprint.paramsKey]] — a version-independent
  * hash of the params (`sha2_256(serialiseData(paramsData))`), since `serialiseData` is
  * consensus-stable. Param-free validators use the empty key.
  *
  * JSON shape (read via ujson, like the Bifrost blueprint), CIP-57-ish plus a `paramsKey`
  * disambiguator:
  * {{{
  *   { "preamble": { "scalusVersion": ..., "scalaVersion": ... },
  *     "validators": [ { "title", "paramsKey", "compiledCode", "hash" }, ... ] }
  * }}}
  */
final class PinnedBlueprint(val json: ujson.Value) {

    private def keyOf(v: ujson.Value): String =
        v.obj.get("paramsKey").map(_.str).getOrElse("")

    private val codeByKey: Map[(String, String), String] =
        json("validators").arr.iterator.map { v =>
            (v("title").str, keyOf(v)) -> v("compiledCode").str
        }.toMap

    private val hashByKey: Map[(String, String), String] =
        json("validators").arr.iterator.map { v =>
            (v("title").str, keyOf(v)) -> v("hash").str
        }.toMap

    /** Fully-applied `compiledCode` hex for `(title, paramsKey)`, if frozen. */
    def lookup(title: String, paramsKey: String): Option[String] =
        codeByKey.get((title, paramsKey))

    /** Frozen script hash hex for `(title, paramsKey)`, if present. */
    def hashOf(title: String, paramsKey: String): Option[String] =
        hashByKey.get((title, paramsKey))
}

object PinnedBlueprint {

    /** The active frozen blueprint the runtime always loads. Pinned to the pre-upgrade toolchain so
      * deployed hashes survive the Scalus/Scala upgrade. Bundled as a classpath resource.
      */
    val ActiveResource: String =
        "/blueprints/binocular-blueprint-scalus-0.18.1-scala-3.3.7.json"

    /** Loaded once from the classpath; `None` if the resource is absent (e.g. before first capture).
      */
    lazy val active: Option[PinnedBlueprint] =
        Option(getClass.getResourceAsStream(ActiveResource)).map { is =>
            try new PinnedBlueprint(ujson.read(is))
            finally is.close()
        }

    /** Empty key for param-free validators. */
    val NoParams: String = ""

    /** Version-independent lookup key for a validator's params. `serialiseData` produces
      * consensus-stable CBOR, so the key does not depend on the compiler.
      */
    def paramsKey(data: Data): String =
        Builtins.sha2_256(Builtins.serialiseData(data)).toHex

    /** Key for a validator parameterized by a sequence of `ByteString` constants (e.g.
      * treasury-movement: `oracleScriptHash, controlNftPolicy, controlNftName`). The arity and
      * lengths are fixed per validator, so concatenation is unambiguous.
      */
    def paramsKeyOf(parts: ByteString*): String =
        paramsKey(Data.B(ByteString.fromArray(parts.toArray.flatMap(_.bytes))))

    /** Canonical validator titles used as blueprint keys. */
    object Titles {
        val Oracle = "oracle"
        val TreasuryMovement = "treasury-movement"
        val Tmtx = "tmtx"
        val PegOutProduced = "peg-out-produced"
        val PegOutNotProduced = "peg-out-not-produced"
        val TxVerifier = "tx-verifier"
        val OneShotMint = "one-shot-mint"
    }

    /** Return the frozen applied script for `(title, paramsKey)`, or fall back to compiling fresh
      * (new deployments / ephemeral regtest / unit tests).
      */
    def pinned(title: String, paramsKey: String)(fresh: => Script.PlutusV3): Script.PlutusV3 =
        active.flatMap(_.lookup(title, paramsKey)) match
            case Some(hex) => Script.PlutusV3(ByteString.fromHex(hex))
            case None      => fresh
}
