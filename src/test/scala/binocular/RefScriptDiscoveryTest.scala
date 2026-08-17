package binocular

import binocular.bitcoin.BitcoinNodeConfig
import binocular.cli.CommandHelpers
import binocular.oracle.{CardanoConfig, OracleConfig, WalletConfig}

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ScriptHash, TransactionHash, TransactionInput}

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
}
