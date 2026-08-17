package binocular

import binocular.bitcoin.BitcoinNodeConfig
import binocular.cli.CommandHelpers
import binocular.oracle.{CardanoConfig, OracleConfig, WalletConfig}

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.{Credential, ScriptHash, Timelock, TransactionHash, TransactionInput}

/** Pins the pure parsing of Blockfrost `/addresses/{addr}/utxos` JSON into the
  * `(reference_script_hash -> outpoint)` pairs used to discover CIP-33 reference-script UTxOs by
  * script hash (so the bridge no longer needs the outpoints recorded in config), and the base URL
  * those scans are issued against.
  */
class RefScriptDiscoveryTest extends AnyFunSuite {

    private val hashA = "a" * 56 // 28-byte script hash
    private val hashB = "b" * 56
    private val tx1 = "1" * 64 // 32-byte tx hash
    private val tx2 = "2" * 64

    private def utxo(txHash: String, idx: Int, refHash: String | Null): ujson.Value = {
        val base = ujson.Obj(
          "tx_hash" -> txHash,
          "output_index" -> idx
        )
        base("reference_script_hash") = if refHash == null then ujson.Null else ujson.Str(refHash)
        base
    }

    test("keeps ref-script UTxOs as (scriptHash -> outpoint), dropping non-ref UTxOs") {
        val items = Seq(
          utxo(tx1, 0, hashA),
          utxo(tx2, 3, hashB),
          utxo(tx1, 1, null) // plain wallet UTxO, no reference script
        )

        val pairs = CommandHelpers.parseRefScriptOutpoints(items)

        assert(
          pairs.toSet == Set(
            ScriptHash.fromHex(hashA) -> TransactionInput(TransactionHash.fromHex(tx1), 0),
            ScriptHash.fromHex(hashB) -> TransactionInput(TransactionHash.fromHex(tx2), 3)
          )
        )
    }

    test("preserves duplicate UTxOs carrying the same script hash") {
        val items = Seq(utxo(tx1, 0, hashA), utxo(tx2, 0, hashA))

        val outpoints = CommandHelpers.parseRefScriptOutpoints(items).map(_._2).toSet

        assert(
          outpoints == Set(
            TransactionInput(TransactionHash.fromHex(tx1), 0),
            TransactionInput(TransactionHash.fromHex(tx2), 0)
          )
        )
    }

    /** Minimal config: only `cardano` matters to the scan decisions, every other section keeps its
      * defaults.
      */
    private def config(
        network: String,
        blockfrostUrl: Option[String] = None,
        projectId: String = "",
        backend: String = "blockfrost"
    ): BinocularConfig =
        BinocularConfig(
          bitcoinNode = BitcoinNodeConfig(),
          cardano = CardanoConfig(
            network = network,
            backend = backend,
            blockfrostProjectId = projectId,
            blockfrostUrl = blockfrostUrl
          ),
          wallet = WalletConfig(),
          oracle = OracleConfig()
        )

    private val addr = "addr_test1qq"

    test("blockfrostBaseUrl prefers configured blockfrost-url") {
        val cfg = config("preprod", Some("http://127.0.0.1:3000"))
        assert(CommandHelpers.blockfrostBaseUrl(cfg) == "http://127.0.0.1:3000")
    }

    test("blockfrostBaseUrl drops a trailing slash from the configured URL") {
        val cfg = config("preprod", Some("http://127.0.0.1:3000/"))
        assert(CommandHelpers.blockfrostBaseUrl(cfg) == "http://127.0.0.1:3000")
    }

    test("blockfrostBaseUrl falls back to hosted per-network URL") {
        assert(
          CommandHelpers.blockfrostBaseUrl(config("preprod")) ==
              "https://cardano-preprod.blockfrost.io/api/v0"
        )
        assert(
          CommandHelpers.blockfrostBaseUrl(config("mainnet")) ==
              "https://cardano-mainnet.blockfrost.io/api/v0"
        )
        assert(
          CommandHelpers.blockfrostBaseUrl(config("preview")) ==
              "https://cardano-preview.blockfrost.io/api/v0"
        )
    }

    test("canScanAddressUtxos allows a self-hosted backend with no project id") {
        // Dolos without a Blockfrost account: `blockfrost-url` set, project id empty. This is
        // exactly what CardanoConfig.validate() admits, so discovery must not bail out here.
        val cfg = config("preprod", blockfrostUrl = Some("http://127.0.0.1:3000"))
        assert(CommandHelpers.canScanAddressUtxos(cfg, addr))
    }

    test("canScanAddressUtxos needs a project id when there is no URL override") {
        assert(!CommandHelpers.canScanAddressUtxos(config("preprod"), addr))
        assert(
          CommandHelpers.canScanAddressUtxos(config("preprod", projectId = "preprodABC"), addr)
        )
    }

    test("canScanAddressUtxos rejects a non-blockfrost backend or an empty address") {
        val hosted = config("preprod", projectId = "preprodABC")
        assert(!CommandHelpers.canScanAddressUtxos(hosted, ""))
        assert(
          !CommandHelpers.canScanAddressUtxos(
            config("preprod", projectId = "preprodABC", backend = "yaci"),
            addr
          )
        )
    }

    test("blockfrostProjectIdHeader falls back to the provider's self-hosted placeholder") {
        val selfHosted = config("preprod", blockfrostUrl = Some("http://127.0.0.1:3000"))
        assert(CommandHelpers.blockfrostProjectIdHeader(selfHosted) == "self-hosted")
        assert(
          CommandHelpers.blockfrostProjectIdHeader(config("preprod", projectId = "preprodABC")) ==
              "preprodABC"
        )
    }

    /** The preprod sponsor wallet base address. */
    private val sponsor = Address.fromBech32(
      "addr_test1qzwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clq68fray"
    )

    test(
      "refScriptHoldingAddress derives a deterministic enterprise script address from the sponsor key"
    ) {
        val script = CommandHelpers.refScriptHoldingScript(sponsor)
        val holding = CommandHelpers.refScriptHoldingAddress(sponsor.getNetwork.get, sponsor)
        assert(holding.isEnterprise)
        assert(holding.hasScript)
        assert(holding.scriptHashOption.contains(script.scriptHash))
        assert(holding.encode.get != sponsor.encode.get)
        // determinism: derive twice, same result
        assert(holding == CommandHelpers.refScriptHoldingAddress(sponsor.getNetwork.get, sponsor))
        // Pinned: the holding address is derived, never configured, so deploy and discovery must
        // agree across releases. A change here relocates every already-deployed reference UTxO.
        assert(
          holding.encode.get == "addr_test1wrr69xp4fm6u9zjjul82ux4v3344r0phtrp322uan5tz4yg7fz5kf"
        )
    }

    test("refScriptHoldingScript is sig(sponsor payment key hash), so the wallet can reclaim") {
        val keyHash = sponsor match {
            case ShelleyAddress(_, ShelleyPaymentPart.Key(hash), _) => hash
            case other => fail(s"fixture sponsor address is not key-based: $other")
        }
        assert(CommandHelpers.refScriptHoldingScript(sponsor).script == Timelock.Signature(keyHash))
    }

    test("refScriptHoldingScript rejects an address with no payment key hash") {
        val scriptAddr = Address(
          sponsor.getNetwork.get,
          Credential.ScriptHash(CommandHelpers.refScriptHoldingScript(sponsor).scriptHash)
        )
        intercept[IllegalArgumentException](CommandHelpers.refScriptHoldingScript(scriptAddr))
    }

    test("refScriptScanAddresses returns holding address first, sponsor second, no duplicates") {
        val addrs = CommandHelpers.refScriptScanAddresses(
          config("preprod", projectId = "preprodABC"),
          sponsor.getNetwork.get,
          sponsor
        )
        assert(addrs.size == 2)
        assert(
          addrs.head == CommandHelpers
              .refScriptHoldingAddress(sponsor.getNetwork.get, sponsor)
              .encode
              .get
        )
        assert(addrs.last == sponsor.encode.get)
    }
}
