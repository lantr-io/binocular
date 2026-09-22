package binocular.cli

import org.scalatest.funsuite.AnyFunSuite
import binocular.BinocularConfig
import binocular.bitcoin.BitcoinNodeConfig
import binocular.oracle.{CardanoConfig, OracleConfig, WalletConfig}
import binocular.cli.commands.TrafficAddressCommand
import java.io.{ByteArrayOutputStream, PrintStream}

class TrafficAddressCommandTest extends AnyFunSuite {
    test("traffic-address parses without arguments") {
        assert(CliApp.command.parse(Seq("traffic-address")).isRight)
    }

    private val fixture =
        ujson.read(os.read(os.pwd / "src/test/resources/fixtures/bip86-wallet-vectors.json"))
    private val mnemonic = fixture("mnemonic").str
    private val config = BinocularConfig(
      BitcoinNodeConfig(network = "testnet4"),
      CardanoConfig(network = "preprod"),
      WalletConfig(mnemonic),
      OracleConfig()
    )

    private def capture(action: => Int): (Int, String, String) = {
        val out = new ByteArrayOutputStream()
        val err = new ByteArrayOutputStream()
        val code = scala.Console.withOut(new PrintStream(out)) {
            scala.Console.withErr(new PrintStream(err)) {
                action
            }
        }
        (code, out.toString("UTF-8"), err.toString("UTF-8"))
    }

    private def run(c: BinocularConfig): (Int, String, String) =
        capture(TrafficAddressCommand().execute(c))

    test("the real CLI routes --config traffic-address without chain setup") {
        val file = os.temp(
          contents = s"""binocular {
          bitcoin-node { network = testnet4, url = "", username = "", password = "" }
          cardano { network = preprod, blockfrost-project-id = "" }
          wallet.mnemonic = "$mnemonic"
        }""",
          suffix = ".conf"
        )
        try {
            assert(
              capture(CliApp.run(Seq("--config", file.toString, "traffic-address"))) == run(config)
            )
        } finally os.remove(file)
    }

    test("offline execution prints only funding address, with no RPC or provider configuration") {
        val expected = fixture("vectors").arr
            .find(v => v("network").str == "testnet" && v("index").num == 1)
            .get
        assert(run(config) == (0, expected("address").str + System.lineSeparator(), ""))
    }

    test("demo command refuses either mainnet and unknown networks") {
        val configs = Seq(
          config.copy(bitcoinNode = BitcoinNodeConfig(network = "mainnet")),
          config.copy(bitcoinNode = BitcoinNodeConfig(network = "typo")),
          config.copy(cardano = CardanoConfig(network = "mainnet")),
          config.copy(cardano = CardanoConfig(network = "typo"))
        )
        for c <- configs do {
            val (code, out, err) = run(c)
            assert(code == 1 && out.isEmpty && err.nonEmpty)
            assert(!err.contains("abandon"))
        }
    }

    test("invalid mnemonic returns a sanitized command error without echoing input") {
        val (code, out, err) =
            run(config.copy(wallet = WalletConfig("private-input invalid mnemonic")))
        assert(code == 1 && out.isEmpty)
        assert(
          err == "Cannot derive Bitcoin wallet: invalid mnemonic or key derivation failed" + System
              .lineSeparator()
        )
    }
}
