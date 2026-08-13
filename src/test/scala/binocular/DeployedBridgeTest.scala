package binocular

import binocular.cli.commands.DeployedBridge
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{TransactionHash, TransactionInput}
import scalus.uplc.builtin.ByteString

/** `deploy-bridge` prints the values a deployment is configured from, and until now printing was
  * the ONLY way to get them: an in-process caller (the federation integration suite) had to scrape
  * stdout. [[DeployedBridge]] is that same set as a value.
  *
  * The record and the printed lines share one formatter precisely so they cannot drift — an
  * operator copying `federation-one-shot-ref` and a test reading the field must get identical
  * bytes, or the two disagree about which bridge was deployed.
  */
class DeployedBridgeTest extends AnyFunSuite {

    private val txHash = "ab" * 32
    private val ref = TransactionInput(TransactionHash.fromHex(txHash), 3)

    private def sample = DeployedBridge(
      configNftPolicyId = ByteString.fromHex("00" * 28),
      configNftAssetName = ByteString.fromString("BIFCFG"),
      bridgedTokenPolicyId = ByteString.fromHex("11" * 28),
      completedPegInsPolicyId = ByteString.fromHex("22" * 28),
      bridgeStatePolicyId = ByteString.fromHex("33" * 28),
      tmScriptHash = ByteString.fromHex("44" * 28),
      pegInPolicyId = ByteString.fromHex("55" * 28),
      pegOutPolicyId = ByteString.fromHex("66" * 28),
      spoBansPolicyId = ByteString.fromHex("77" * 28),
      sposRegistryPolicyId = ByteString.fromHex("88" * 28),
      treasuryInfoPolicyId = ByteString.fromHex("99" * 28),
      yFederation = ByteString.fromHex("aa" * 32),
      completedPegInsOneShotRef = ref,
      bridgeStateOneShotRef = ref,
      federationOneShotRef = ref
    )

    test("refString renders the TX_HASH#INDEX form binocular's own config keys take") {
        assert(DeployedBridge.refString(sample.federationOneShotRef) == s"$txHash#3")
    }

    test("heimdallRefString renders the TXID:VOUT form heimdall's toml takes") {
        // heimdall's cardano.registry_bootstrap / treasury_bootstrap use a COLON. Two spellings
        // of one outpoint, and handing either tool the other's form is a silent DerivedMismatch.
        assert(DeployedBridge.heimdallRefString(sample.federationOneShotRef) == s"$txHash:3")
    }

    test("the completion-half one-shot is shared by the CPI trie and the bridge state") {
        // Both are minted in the bootstrap tx against the SAME wallet outpoint, which is why
        // binocular's config carries the identical value under two keys.
        assert(sample.completedPegInsOneShotRef == sample.bridgeStateOneShotRef)
    }
}
