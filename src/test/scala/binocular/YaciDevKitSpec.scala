package binocular

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.{SlotConfig, TransactionHash, TransactionInput, Utxo, Utxos, Value}
import scalus.cardano.node.{BlockchainProvider, BlockfrostProvider}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.hd.HdAccount
import scalus.crypto.ed25519.given
import com.bloxbean.cardano.yaci.test.YaciCardanoContainer
import munit.FunSuite

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*

/** Base trait for integration tests using Yaci DevKit
  *
  * Provides a local Cardano devnet via Docker container for testing smart contracts and
  * transactions in a controlled environment.
  */
trait YaciDevKitSpec extends FunSuite {

    /** Configuration for Yaci DevKit container */
    case class YaciDevKitConfig(
        initialFundingLovelace: Long = 100_000_000_000L, // 100,000 ADA
        enableLogs: Boolean = false
    )

    /** Wrapper for Yaci DevKit container with Scalus provider */
    class YaciDevKit(
        val container: YaciCardanoContainer,
        val hdAccount: HdAccount,
        val config: YaciDevKitConfig
    ) {
        given ec: ExecutionContext = ExecutionContext.global

        private val yaciUrl =
            s"http://${container.getHost}:${container.getMappedPort(8080)}/api/v1"

        /** Get BlockchainProvider connected to Yaci DevKit.
          *
          * Computes the correct SlotConfig by querying the devnet's latest block to determine
          * genesis time, so that Instant.now() maps to a valid devnet slot.
          */
        lazy val provider: BlockchainProvider = {
            val slotConfig = fetchYaciSlotConfig(yaciUrl)
            println(
              s"[YaciDevKit] SlotConfig: zeroTime=${slotConfig.zeroTime}, zeroSlot=${slotConfig.zeroSlot}, slotLength=${slotConfig.slotLength}"
            )
            Await.result(
              BlockfrostProvider.localYaci(baseUrl = yaciUrl, slotConfig = slotConfig),
              30.seconds
            )
        }

        /** Get TransactionSigner */
        lazy val signer: TransactionSigner = new TransactionSigner(Set(hdAccount.paymentKeyPair))

        /** Get sponsor address */
        lazy val sponsorAddress: Address = hdAccount.baseAddress(Network.Testnet)

        /** Get address as bech32 string */
        def baseAddress: String =
            sponsorAddress.asInstanceOf[scalus.cardano.address.ShelleyAddress].toBech32.get

        /** Get UTXOs for an address */
        def getUtxos(address: Address): List[Utxo] = {
            val result = Await.result(provider.findUtxos(address), 30.seconds)
            result match {
                case Right(u) => u.map { case (input, output) => Utxo(input, output) }.toList
                case Left(_)  => List.empty
            }
        }

        /** Get total lovelace balance for an address */
        def getLovelaceBalance(address: Address): BigInt = {
            val utxos = getUtxos(address)
            utxos.map(u => BigInt(u.output.value.coin.value)).sum
        }

        /** Wait for transaction to be confirmed
          *
          * @param txHash
          *   transaction hash to wait for
          * @param maxAttempts
          *   maximum number of attempts (default: 30)
          * @param delayMs
          *   delay between attempts in milliseconds (default: 1000)
          * @return
          *   true if transaction was confirmed, false if timeout
          */
        def waitForTransaction(
            txHash: String,
            maxAttempts: Int = 30,
            delayMs: Long = 1000
        ): Boolean = {
            val input = TransactionInput(TransactionHash.fromHex(txHash), 0)
            var attempts = 0
            while attempts < maxAttempts do {
                try {
                    val result = Await.result(provider.findUtxo(input), 10.seconds)
                    result match {
                        case Right(_) => return true
                        case Left(_) =>
                            Thread.sleep(delayMs)
                            attempts += 1
                    }
                } catch {
                    case _: Exception =>
                        Thread.sleep(delayMs)
                        attempts += 1
                }
            }
            false
        }

        /** Stop the container */
        def stop(): Unit = container.stop()
    }

    /** Fetch the latest block from the Yaci devnet and compute a SlotConfig whose zeroTime maps
      * wall-clock Instant.now() to the devnet's current slot range.
      *
      * The default SlotConfig(0, 0, 1000) sets zeroTime=epoch (1970), which makes Instant.now() map
      * to slot ~1.77 billion — far beyond the devnet's ~1800-slot range. By querying /blocks/latest
      * for the tip's slot and Unix timestamp, we compute zeroTime = (blockTime - blockSlot) * 1000,
      * anchoring the mapping correctly.
      */
    private def fetchYaciSlotConfig(yaciUrl: String): SlotConfig = {
        val slotLength = 1000L // 1 second per slot (Yaci default)
        try {
            val client = HttpClient.newHttpClient()
            val request = HttpRequest
                .newBuilder()
                .uri(URI.create(s"$yaciUrl/blocks/latest"))
                .GET()
                .build()
            val response = client.send(request, HttpResponse.BodyHandlers.ofString())
            val body = response.body()

            // Parse "slot" and "time" from JSON response using simple regex
            // Blockfrost returns: { "slot": 630, "time": 1769705300, ... }
            val slotPattern = """"slot"\s*:\s*(\d+)""".r
            val timePattern = """"time"\s*:\s*(\d+)""".r

            val slot = slotPattern.findFirstMatchIn(body).map(_.group(1).toLong).getOrElse(0L)
            val time = timePattern.findFirstMatchIn(body).map(_.group(1).toLong).getOrElse(0L)

            if slot > 0 && time > 0 then {
                // zeroTime = block's Unix time (ms) minus slot * slotLength
                val zeroTimeMs = time * 1000L - slot * slotLength
                println(
                  s"[YaciDevKit] Latest block: slot=$slot, time=$time -> zeroTime=$zeroTimeMs"
                )
                SlotConfig(zeroTimeMs, 0L, slotLength)
            } else {
                println(
                  s"[YaciDevKit] Could not parse tip block (slot=$slot, time=$time), falling back to wall-clock estimate"
                )
                // Fallback: assume genesis was ~30s before now (typical container startup)
                val zeroTimeMs = System.currentTimeMillis() - 30 * slotLength
                SlotConfig(zeroTimeMs, 0L, slotLength)
            }
        } catch {
            case e: Exception =>
                println(
                  s"[YaciDevKit] Error fetching tip block: ${e.getMessage}, falling back to wall-clock estimate"
                )
                val zeroTimeMs = System.currentTimeMillis() - 30 * slotLength
                SlotConfig(zeroTimeMs, 0L, slotLength)
        }
    }

    /** Create and start a Yaci DevKit container */
    def createYaciDevKit(config: YaciDevKitConfig = YaciDevKitConfig()): YaciDevKit = {
        val mnemonic =
            "test test test test test test test test test test test test test test test test test test test test test test test sauce"
        val hdAccount = HdAccount.fromMnemonic(mnemonic, "", accountIndex = 0)

        val container = new YaciCardanoContainer()
        container.withCreateContainerCmdModifier(cmd => cmd.withName("binocular-yaci-devkit"))
        container.withReuse(true)

        if config.enableLogs then {
            container.withLogConsumer(frame => println(s"[Yaci] ${frame.getUtf8String}"))
        }

        container.start()

        new YaciDevKit(container, hdAccount, config)
    }

    /** Execute test with Yaci DevKit container */
    def withYaciDevKit[T](
        config: YaciDevKitConfig = YaciDevKitConfig()
    )(testFn: YaciDevKit => T): T = {
        val devKit = createYaciDevKit(config)
        try {
            testFn(devKit)
        } finally {
            devKit.stop()
        }
    }

    /** Fixture for Yaci DevKit that can be reused across multiple tests */
    val yaciDevKitFixture = new Fixture[YaciDevKit]("yaci-devkit") {
        private var devKit: YaciDevKit = _

        def apply(): YaciDevKit = devKit

        override def beforeAll(): Unit = {
            devKit = createYaciDevKit(YaciDevKitConfig(enableLogs = false))
        }

        override def afterAll(): Unit = {
            if devKit != null then {
                devKit.stop()
            }
        }
    }
}
