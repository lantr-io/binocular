package binocular.federation

import binocular.bitcoin.BitcoinNodeConfig
import org.scalatest.funsuite.AnyFunSuite

/** The scenario's config, checked without starting anything.
  *
  * Worth its own suite because a wrong value here does not fail where it is set — it fails ten
  * minutes later as a provider that cannot connect, or an oracle that will not promote, or a
  * treasury address holding nothing.
  */
class DevnetConfigTest extends AnyFunSuite {

    private val bitcoin = BitcoinNodeConfig(
      url = "http://127.0.0.1:18443",
      username = "test",
      password = "test",
      network = "regtest"
    )

    private def sample = DevnetConfig(
      yaciStoreUrl = "http://localhost:59761/api/v1",
      yaciAdminUrl = "http://localhost:59759/local-cluster/api",
      bitcoin = bitcoin,
      yFederationHex = "b1" * 32,
      stateDir = os.temp.dir(prefix = "devnet-cfg-")
    )

    test("the devnet mnemonic is a valid 24-word HD account") {
        // Wrong by one word and every command fails as "cannot create wallet", with no hint that
        // the mnemonic is the cause.
        assert(DevnetConfig.DevnetMnemonic.split("\\s+").length == 24)
        val account = sample.wallet.createHdAccount()
        assert(account.isRight, s"mnemonic rejected: ${account.left.toOption}")
    }

    test("the Cardano backend routes at the devkit, not Blockfrost") {
        val c = sample.cardano
        assert(c.backend == "yaci", "the yaci backend takes the two container URLs directly")
        assert(c.yaciStoreUrl.endsWith("/api/v1"))
        assert(c.yaciAdminUrl.endsWith("/local-cluster/api"))
        assert(c.scalusNetwork == scalus.cardano.address.Network.Testnet)
        // NOT asserted here: that a provider can be constructed. `createBlockchainProvider`
        // CONNECTS, so against the placeholder URLs above it returns Left for a reason that has
        // nothing to do with the config's shape. The genesis test makes that call against the
        // real container, which is the only place the answer means anything.
    }

    test("oracle parameters match the scenario: 3-block fork tree, immediate promotion") {
        val o = sample.oracle
        assert(o.maxBlocksInForkTree == 3)
        assert(o.maturationConfirmations == 0, "a test cannot wait 100 blocks per promotion")
        assert(!o.testingMode, "real regtest PoW headers, as the existing regtest suite validates")
    }

    test("the refund timeout exceeds the federation CSV delay, as [CFG-9] requires") {
        // heimdall REFUSES a Config whose refund window opens before the federation's sweep. Set
        // wrongly here, genesis publishes a datum every heimdall then rejects at startup.
        val b = sample.bridge
        assert(b.peginRefundTimeoutBlocks > b.federationCsvBlocks)
    }

    test("the federation key is carried as genesis input, and nothing genesis produces is preset") {
        val b = sample.bridge
        assert(b.yFederationHex == "b1" * 32)
        // These are OUTPUTS of deploy-bridge. Preset values would make a stale deployment look
        // configured and derive policies from the wrong outpoints.
        assert(b.federationOneShotRef.isEmpty)
        assert(b.bridgeStateOneShotRef.isEmpty)
        assert(b.completedPegInsOneShotRef.isEmpty)
        assert(b.configNftAssetName.isEmpty || b.configNftPolicyId.forall(_ == '0'))
    }

    test("background daemons are off so nothing runs underneath an assertion") {
        assert(!sample.bridge.porSweeper)
        assert(!sample.bridge.proofServer)
    }
}
