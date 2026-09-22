package binocular

import binocular.bitcoin.*
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.{TransactionOutPoint, WitnessTransaction}
import org.bitcoins.crypto.{DoubleSha256Digest, SchnorrDigitalSignature, SchnorrPublicKey}
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector

class Bip86WalletTest extends AnyFunSuite {
    private val fixture =
        ujson.read(os.read(os.pwd / "src/test/resources/fixtures/bip86-wallet-vectors.json"))
    private val mnemonic = fixture("mnemonic").str
    private def wallet(network: BitcoinNetwork = BitcoinNetwork.Testnet4): Bip86Wallet =
        Bip86Wallet.fromMnemonic(mnemonic, network).fold(fail(_), identity)
    private def verify(key: ByteVector, hash: ByteVector, sig: ByteVector): Boolean =
        SchnorrPublicKey(key).verify(hash, SchnorrDigitalSignature(sig))

    for (name, network) <- Seq(
          "mainnet" -> BitcoinNetwork.Mainnet,
          "testnet" -> BitcoinNetwork.Testnet,
          "regtest" -> BitcoinNetwork.Regtest
        )
    do {
        test(s"$name fixed roles match independent BIP86 vectors") {
            val w = wallet(network)
            val expected = fixture("vectors").arr.filter(_("network").str == name)
            for (key, vector) <- Seq(w.identity, w.funding).zip(expected) do {
                assert(key.path == vector("path").str)
                assert(key.internalKey.toHex == vector("internal_key").str)
                assert(key.outputKey.toHex == vector("output_key").str)
                assert(key.scriptPubKey.toHex == vector("script_pub_key").str)
                assert(key.address == vector("address").str)
            }
            assert(w.identity.outputKey != w.funding.outputKey)
        }
    }

    test("testnet4 shares testnet keys; whitespace does not change derivation") {
        val spaced = Bip86Wallet
            .fromMnemonic("  " + mnemonic.replace(" ", "\n  ") + "\t", BitcoinNetwork.Testnet4)
            .toOption
            .get
        assert(spaced.funding.address == wallet(BitcoinNetwork.Testnet).funding.address)
    }

    test("invalid mnemonic errors never echo mnemonic words") {
        for invalid <- Seq("", "abandon " * 12, "abandon " * 11 + "sensitive-word", "abandon about")
        do {
            assert(
              Bip86Wallet.fromMnemonic(invalid, BitcoinNetwork.Testnet4) ==
                  Left("Cannot derive Bitcoin wallet: invalid mnemonic or key derivation failed")
            )
        }
        assert(!wallet().toString.contains("abandon"))
        assert(!wallet().identity.toString.contains("abandon"))
    }

    test("BIP322 signs under Q_auth, not the raw HD child") {
        for network <- Seq(BitcoinNetwork.Mainnet, BitcoinNetwork.Testnet4) do {
            val w = wallet(network)
            for key <- Seq(w.identity, w.funding) do {
                val message = ByteVector.fill(32)(42.toByte)
                val sig = key.signMessage(message)
                assert(sig.size == 64)
                assert(verify(key.outputKey, Bip322.keypathSighash(message, key.outputKey), sig))
                assert(!verify(key.internalKey, Bip322.keypathSighash(message, key.outputKey), sig))
            }
        }
    }

    test("the funding role signs a real deposit") {
        val w = wallet()
        val v = TaprootVectors.tupleA
        val tree = PeginTreeParams(
          ByteVector.fromValidHex(v("y51").str),
          ByteVector.fromValidHex(v("y_federation").str),
          v("federation_csv_blocks").num.toInt,
          v("pegin_refund_timeout_blocks").num.toInt
        )
        val coin = ScannedBitcoinOutput(
          TransactionOutPoint(DoubleSha256Digest(ByteVector.fill(32)(2.toByte)), UInt32(0)),
          100000,
          w.funding.scriptPubKey,
          90,
          false
        )
        val tx = PegInDeposit
            .build(tree, w.identity.outputKey, 30000, Seq(coin), w.funding, 1000)
            .fold(fail(_), identity)
            .asInstanceOf[WitnessTransaction]
        assert(tx.outputs.head.value.satoshis.toLong == 30000)
        assert(tx.witness.head.stack.map(_.size) == Vector(64))
    }
}
