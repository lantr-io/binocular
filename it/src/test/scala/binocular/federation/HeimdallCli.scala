package binocular.federation

import scalus.uplc.builtin.ByteString

/** Typed wrappers over the heimdall commands the scenario drives.
  *
  * heimdall is a separate binary, so its interface here is argv in and stdout out. Parsing is
  * confined to this file: a scenario step should read as "get the group key", not as a regex over
  * somebody else's log format, and when heimdall reworded a line exactly one place should need
  * changing.
  */
object HeimdallCli {

    /** What `frost-treasury` establishes about a bridge before it exists.
      *
      * @param groupKey
      *   Y_51, the FROST group key the roster signs under
      * @param yFederation
      *   the recovery-leaf key; equal to `groupKey` at genesis, the collapsed Phase-1 convention
      * @param treasuryAddress
      *   the P2TR address the genesis BTC must be sent to
      * @param scriptPubKey
      *   that address's scriptPubKey, hex
      */
    case class FrostTreasury(
        groupKey: ByteString,
        yFederation: ByteString,
        treasuryAddress: String,
        scriptPubKey: String
    )

    /** Derive the group key and the treasury address, with no chain and no ceremony.
      *
      * With no `--frost-key`, `frost-treasury` reproduces the deterministic demo DKG — and its
      * result is byte-identical to what three `heimdall run-spo` instances converge on over HTTP
      * (verified against the key `scripts/dkz/demo-spo-{1,2,3}.sh` document). That equality is what
      * lets genesis publish `y_federation` BEFORE the SPOs have run their ceremony, and still have
      * the first TM's treasury input verify: the roster re-derives the same key.
      *
      * So this replaces what would otherwise be a rehearsal phase — three processes on a mock
      * chain, killed once their logs revealed the key.
      *
      * @param config
      *   a heimdall TOML supplying `demo.min_signers` / `demo.max_signers`; the derivation is a
      *   function of those and the fixed demo seed, so a 2-of-3 file yields the 2-of-3 key
      */
    def frostTreasury(
        config: os.Path,
        federationCsvBlocks: Int,
        yFederation: Option[ByteString] = None
    ): FrostTreasury = {
        val args = Seq(
          "frost-treasury",
          "--config",
          config.toString,
          "--federation-csv-blocks",
          federationCsvBlocks.toString
        ) ++ yFederation.toSeq.flatMap(k => Seq("--y-federation", k.toHex))

        val res = os.proc(HeimdallBuild.binary(), args).call(check = false)
        val out = res.out.text()
        require(
          res.exitCode == 0,
          s"heimdall frost-treasury failed (exit ${res.exitCode}):\n$out\n${res.err.text()}"
        )

        FrostTreasury(
          groupKey = ByteString.fromHex(field(out, "FROST group key \\(x-only\\)")),
          yFederation = ByteString.fromHex(field(out, "y_federation \\(leaf key\\)")),
          treasuryAddress = field(out, "Treasury address"),
          scriptPubKey = field(out, "Script pubkey")
        )
    }

    /** One `Label: value` line of heimdall's output.
      *
      * Trailing commentary is stripped — `frost-treasury` appends "(defaulted to Y_51 — genesis
      * tree)" to the leaf key when no `--y-federation` is given, and that annotation is information
      * for an operator, not part of the value.
      */
    private def field(out: String, label: String): String = {
        val re = s"$label:\\s*(\\S+)".r
        re.findFirstMatchIn(out)
            .map(_.group(1))
            .getOrElse(
              throw new RuntimeException(
                s"heimdall output has no `$label:` line — the command's format changed. Got:\n$out"
              )
            )
    }
}
