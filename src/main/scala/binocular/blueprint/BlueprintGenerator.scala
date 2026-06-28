package binocular.blueprint

import binocular.BinocularConfig
import binocular.oracle.BitcoinContract
import binocular.watchtower.*

import scalus.cardano.ledger.Script
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData

import java.nio.file.{Files, Path}

/** Generates the frozen, fully-applied blueprint (Strategy A): for each binocular-authored
  * validator it records the *final* compiled UPLC bytes (and script hash), so the runtime can load
  * them verbatim and deployed hashes survive compiler upgrades. See [[PinnedBlueprint]].
  *
  * Param-free validators contribute one deployment-independent entry each. The parameterized ones
  * (oracle, treasury-movement) contribute one keyed entry per deployment config; run the generator
  * against each live network config and merge.
  */
object BlueprintGenerator {

    final case class Entry(title: String, paramsKey: String, compiledCode: String, hash: String)

    private def entry(title: String, paramsKey: String, script: Script.PlutusV3): Entry =
        Entry(title, paramsKey, script.script.toHex, script.scriptHash.toHex)

    /** Param-free validators — deployment-independent, one entry each. */
    def paramFreeEntries(): Seq[Entry] = Seq(
      entry(PinnedBlueprint.Titles.Tmtx, PinnedBlueprint.NoParams, TmtxScript.mintingScript.script),
      entry(
        PinnedBlueprint.Titles.PegOutProduced,
        PinnedBlueprint.NoParams,
        PegOutProducedVerifierContract.compiled.script
      ),
      entry(
        PinnedBlueprint.Titles.PegOutNotProduced,
        PinnedBlueprint.NoParams,
        PegOutNotProducedVerifierContract.compiled.script
      ),
      entry(
        PinnedBlueprint.Titles.TxVerifier,
        PinnedBlueprint.NoParams,
        Script.PlutusV3(TransactionVerifierContract.validator.cborByteString)
      )
    )

    /** Deployment-specific keyed entries derived from a config's oracle + bridge params. Empty when
      * the oracle is not configured.
      */
    def deploymentEntries(config: BinocularConfig): Seq[Entry] =
        config.oracle.toBitcoinValidatorParams(config.bitcoinNode.bitcoinNetwork) match {
            case Right(params) =>
                val oracleScript = BitcoinContract.makeContract(params).script
                val oracleHash = ByteString.fromArray(oracleScript.scriptHash.bytes)
                val tmPolicy = ByteString.fromHex(config.bridge.tmControlNftPolicy)
                val tmName = ByteString.fromHex(config.bridge.tmControlNftName)
                val tmScript =
                    TreasuryMovementContract.contract(oracleHash, tmPolicy, tmName).script
                Seq(
                  entry(
                    PinnedBlueprint.Titles.Oracle,
                    PinnedBlueprint.paramsKey(params.toData),
                    oracleScript
                  ),
                  entry(
                    PinnedBlueprint.Titles.TreasuryMovement,
                    PinnedBlueprint.paramsKeyOf(oracleHash, tmPolicy, tmName),
                    tmScript
                  )
                )
            case Left(_) => Seq.empty
        }

    /** All entries for one config: param-free + deployment-specific. */
    def entriesFor(config: BinocularConfig): Seq[Entry] =
        paramFreeEntries() ++ deploymentEntries(config)

    def toJson(entries: Seq[Entry], scalusVersion: String, scalaVersion: String): ujson.Obj =
        ujson.Obj(
          "preamble" -> ujson.Obj(
            "title" -> "Binocular pinned validators (fully-applied)",
            "scalusVersion" -> scalusVersion,
            "scalaVersion" -> scalaVersion
          ),
          "validators" -> ujson.Arr(
            entries.map(e =>
                ujson.Obj(
                  "title" -> e.title,
                  "paramsKey" -> e.paramsKey,
                  "compiledCode" -> e.compiledCode,
                  "hash" -> e.hash
                )
            )*
          )
        )

    def parse(json: ujson.Value): Seq[Entry] =
        json("validators").arr.toSeq.map(v =>
            Entry(
              v("title").str,
              v.obj.get("paramsKey").map(_.str).getOrElse(""),
              v("compiledCode").str,
              v("hash").str
            )
        )

    /** Merge `add` into `existing`, replacing by `(title, paramsKey)`, stable-sorted for a
      * deterministic file.
      */
    def merge(existing: Seq[Entry], add: Seq[Entry]): Seq[Entry] = {
        val byKey = scala.collection.mutable.LinkedHashMap.empty[(String, String), Entry]
        (existing ++ add).foreach(e => byKey((e.title, e.paramsKey)) = e)
        byKey.values.toSeq.sortBy(e => (e.title, e.paramsKey))
    }

    /** Blueprint file name for a toolchain, e.g.
      * `binocular-blueprint-scalus-0.18.1-scala-3.3.7.json`.
      */
    def fileName(scalusVersion: String, scalaVersion: String): String =
        s"binocular-blueprint-scalus-$scalusVersion-scala-$scalaVersion.json"

    /** Generate/merge entries for `config` and write the versioned blueprint to `dir`. Returns the
      * written path.
      */
    def writeMerged(
        dir: Path,
        config: BinocularConfig,
        scalusVersion: String,
        scalaVersion: String
    ): Path = {
        val path = dir.resolve(fileName(scalusVersion, scalaVersion))
        val existing =
            if Files.exists(path) then parse(ujson.read(Files.readString(path))) else Seq.empty
        val merged = merge(existing, entriesFor(config))
        Files.createDirectories(dir)
        Files.writeString(
          path,
          ujson.write(toJson(merged, scalusVersion, scalaVersion), indent = 2)
        )
        path
    }
}
