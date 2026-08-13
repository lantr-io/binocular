package binocular.federation

import binocular.BinocularConfig
import binocular.bitcoin.BitcoinNodeConfig
import binocular.oracle.{CardanoConfig, OracleConfig, WalletConfig}
import binocular.watchtower.BridgeConfig

/** Builds the [[BinocularConfig]] the scenario's in-process commands run against.
  *
  * Every binocular command takes a `BinocularConfig` and nothing else, so this is the whole
  * interface between the test harness and the bridge tooling. Built in code rather than loaded
  * from a HOCON fixture: half these values (container ports, bitcoind's allocated RPC port, temp
  * directories, the one-shot refs genesis produces) do not exist until the suite is running, and a
  * fixture file would have to be rewritten mid-run to carry them.
  */
object DevnetConfig {

    /** The yaci devkit's funded genesis wallet — the standard test mnemonic every devkit ships. */
    val DevnetMnemonic: String =
        "test test test test test test test test test test test test " +
            "test test test test test test test test test test test sauce"

    /** Oracle parameters for the scenario (spec: 3-block fork tree, 0 confirmation timeout).
      *
      *   - `maxBlocksInForkTree = 3` keeps the tree small enough to reason about by hand when a
      *     reorg test is added later; the default 256 would make every fork-tree assertion a
      *     needle in a haystack.
      *   - `maturationConfirmations = 0` promotes a block the moment it is relayed. Mainnet waits
      *     100 (~16 h); a test that waited even 6 would spend its whole budget mining.
      *   - `testingMode = false` — real regtest PoW headers are validated, as
      *     `BinocularRegtestIntegrationTest` already does. Turning it off would make the oracle
      *     accept headers no real chain would, which is most of what the oracle exists to refuse.
      */
    def oracle(
        txOutRef: String = "",
        scriptHash: String = "",
        ownerPkh: String = ""
    ): OracleConfig = OracleConfig(
      txOutRef = txOutRef,
      scriptHash = scriptHash,
      ownerPkh = ownerPkh,
      maxBlocksInForkTree = 3,
      maturationConfirmations = 0,
      testingMode = false,
      // Poll fast: a devnet produces blocks in seconds, and the defaults are tuned for a chain
      // that does not.
      pollInterval = 1,
      retryInterval = 2,
      transactionTimeout = 120
    )

    /** The Cardano half: the devkit container's own two URLs.
      *
      * `backend = "yaci"` routes `createBlockchainProvider` at `localYaci(storeUrl, adminUrl)`, so
      * no Blockfrost project id is involved on binocular's side. heimdall is the opposite - it
      * speaks Blockfrost - and takes the store URL as its base with any project id.
      */
    def cardano(yaciStoreUrl: String, yaciAdminUrl: String): CardanoConfig = CardanoConfig(
      network = "testnet",
      backend = "yaci",
      yaciStoreUrl = yaciStoreUrl,
      yaciAdminUrl = yaciAdminUrl
    )

    /** The bridge half, before genesis has produced anything.
      *
      * `yFederationHex` is the ceremony's `federation_setup_Y`: an INPUT to genesis, since the
      * treasury address the anchor is funded at derives from it. Everything genesis produces -
      * the one-shot refs, the config NFT policy - is absent here and filled in afterwards from
      * the `DeployedBridge` record.
      */
    def bridge(
        yFederationHex: String,
        stateDir: os.Path,
        initialBtcTreasuryUtxo: String = "",
        initialBtcTreasuryAmountSat: Long = 0L
    ): BridgeConfig = BridgeConfig(
      yFederationHex = yFederationHex,
      federationCsvBlocks = 144,
      // > federationCsvBlocks, which heimdall enforces at [CFG-9]: the federation's sweep window
      // must open before the depositor's refund does.
      peginRefundTimeoutBlocks = 720,
      initialBtcTreasuryUtxo = initialBtcTreasuryUtxo,
      initialBtcTreasuryAmountSat = initialBtcTreasuryAmountSat,
      stateDir = stateDir.toString,
      // The watchtower's own daemons are driven explicitly by the scenario, one step at a time,
      // so nothing runs on a timer underneath an assertion.
      porSweeper = false,
      proofServer = false
    )

    def apply(
        yaciStoreUrl: String,
        yaciAdminUrl: String,
        bitcoin: BitcoinNodeConfig,
        yFederationHex: String,
        stateDir: os.Path
    ): BinocularConfig = BinocularConfig(
      bitcoinNode = bitcoin,
      cardano = cardano(yaciStoreUrl, yaciAdminUrl),
      wallet = WalletConfig(mnemonic = DevnetMnemonic),
      oracle = oracle(),
      bridge = bridge(yFederationHex, stateDir)
    )
}
