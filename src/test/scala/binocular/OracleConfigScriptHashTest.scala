package binocular

import binocular.bitcoin.BitcoinNetwork
import binocular.oracle.{BitcoinContract, OracleConfig}

import org.scalatest.funsuite.AnyFunSuite

class OracleConfigScriptHashTest extends AnyFunSuite {

    private val config = OracleConfig(
      txOutRef = "1ad86991a316bb022f0192435e7fdc36c06b93359ee423a28c0cc52165d0564b#0",
      ownerPkh = "c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee8"
    )

    private lazy val derivedHash = {
        val params = config.toBitcoinValidatorParams(BitcoinNetwork.Mainnet).toOption.get
        BitcoinContract.script(params).scriptHash.toHex
    }

    test("empty script-hash skips verification") {
        assert(config.verifyScriptHash(BitcoinNetwork.Mainnet) == Right(()))
    }

    test("matching script-hash passes, case-insensitively") {
        assert(
          config.copy(scriptHash = derivedHash).verifyScriptHash(BitcoinNetwork.Mainnet) ==
              Right(())
        )
        assert(
          config
              .copy(scriptHash = derivedHash.toUpperCase)
              .verifyScriptHash(BitcoinNetwork.Mainnet) == Right(())
        )
    }

    test("mismatched script-hash fails with both hashes in the message") {
        val wrong = "0" * 56
        config.copy(scriptHash = wrong).verifyScriptHash(BitcoinNetwork.Mainnet) match {
            case Left(msg) =>
                assert(msg.contains(wrong))
                assert(msg.contains(derivedHash))
            case Right(_) => fail("expected mismatch to be rejected")
        }
    }
}
