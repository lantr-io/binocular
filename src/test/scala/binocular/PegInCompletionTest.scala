package binocular

import binocular.bitcoin.{Bip86Wallet, BitcoinNetwork}
import binocular.cli.commands.BridgeSweepSetup
import binocular.oracle.WalletConfig
import binocular.server.ProofService.SweptSnapshot
import binocular.watchtower.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.node.Emulator
import scalus.cardano.onchain.plutus.prelude.Option as POption
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.crypto.trie.MerklePatriciaForestry as MPF
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry as OnChainMPF
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import scalus.utils.await
import scodec.bits.ByteVector
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

class PegInCompletionTest extends AnyFunSuite {
    private given ExecutionContext = ExecutionContext.global
    private val timeout = 60.seconds
    private val chainInfo = CardanoInfo.preprod
    private val mnemonic = "abandon " * 11 + "about"
    private val sponsor = WalletConfig(mnemonic).createHdAccount().toOption.get
    private val recipient = sponsor.baseAddress(Network.Testnet)
    private val wallet = Bip86Wallet.fromMnemonic(mnemonic, BitcoinNetwork.Testnet).toOption.get
    private def bytes(n: Int, length: Int) = ByteString.fromHex(f"$n%02x" * length)
    private def input(n: Int) = TransactionInput(TransactionHash.fromHex(bytes(n, 32).toHex), 0)
    private def id(n: Int) = bytes(n, 32) ++ ByteString.fromHex("00000000")
    private val configPolicy = bytes(21, 28)
    private val blueprint = BifrostBlueprint.packaged
    private val pegIn = PegInContract(blueprint, bytes(22, 28), configPolicy)
    private val cpi =
        CompletedPegInsContract(blueprint, configPolicy, TxOutRef(TxId(bytes(23, 32)), 0))
    private val token = BridgedTokenContract(blueprint, configPolicy)
    private val completion =
        PegInCompletion(pegIn, cpi, token, AssetName(ConfigDatum.BridgedTokenAssetName))
    private val priorId = id(1)
    private val depositId = id(2)
    private val sweptValue = id(12)
    private val swept = MPF.empty.insert(priorId, id(11)).insert(depositId, sweptValue)
    private val completed = MPF.empty.insert(priorId, id(11))
    private val datum = PegInDatum(
      AuthorizationMethod.CardanoSignature(ByteString.empty),
      ByteString.empty,
      0,
      depositId,
      30000,
      ByteString.fromArray(wallet.identity.outputKey.toArray),
      0
    )
    private val config = ConfigDatum(
      POption.None,
      ConfigParams(ScheduleParams(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 1, 1000, 10000, 0, 0, 0, 144, 720),
      ByteString.fromArray(token.policyId.bytes),
      ByteString.fromArray(cpi.policyId.bytes),
      bytes(24, 28),
      bytes(25, 28),
      ByteString.fromArray(pegIn.policyId.bytes),
      bytes(26, 28),
      bytes(27, 28),
      bytes(28, 28),
      bytes(29, 28),
      bytes(30, 32),
      TxOutRef(TxId(bytes(31, 32)), 0)
    )
    private def scriptAddress(policy: ByteString) =
        Address(Network.Testnet, Credential.ScriptHash(ScriptHash.fromHex(policy.toHex)))
    private def utxo(n: Int, address: Address, value: Value, data: scalus.uplc.builtin.Data) =
        Utxo(input(n), TransactionOutput.Babbage(address, value, Some(DatumOption.Inline(data))))
    private val pir = utxo(
      40,
      pegIn.address(Network.Testnet),
      Value.ada(3) + Value.asset(pegIn.policyId, AssetName(bytes(41, 32)), 1),
      datum.toData
    )
    private val cpiUtxo = utxo(
      42,
      cpi.address(Network.Testnet),
      Value.ada(3) + Value.asset(cpi.policyId, AssetName(CompletedPegInsContract.assetName), 1),
      CompletedPegInsMerkleTreeDatum(completed.rootHash).toData
    )
    private val configUtxo = utxo(
      43,
      scriptAddress(configPolicy),
      Value.ada(3) + Value.asset(
        ScriptHash.fromHex(configPolicy.toHex),
        AssetName(ConfigDatum.ConfigNftAssetName),
        1
      ),
      config.toData
    )
    private val state = BridgeState(swept.rootHash, MPF.empty.rootHash, id(44), 100000)
    private val bss = utxo(
      45,
      scriptAddress(config.bridgeStatePolicy),
      Value.ada(3) + Value.asset(
        ScriptHash.fromHex(config.bridgeStatePolicy.toHex),
        AssetName(TreasuryMovementValidator.BridgeStateAssetName),
        1
      ),
      state.toData
    )
    private val snapshot =
        SweptSnapshot(BridgeSweepSetup.SingletonContext(configUtxo, config, bss, state), swept)
    private val refs = Seq(pegIn.script, cpi.script, token.script, pegIn.script).zipWithIndex.map {
        (script, n) =>
            Utxo(
              input(50 + n),
              TransactionOutput.Babbage(
                recipient,
                Value.ada(30),
                scriptRef = Some(ScriptRef(script))
              )
            )
    }
    private val pairs = refs.map(u => u.output.scriptRef.get.script.scriptHash -> u.input)
    private def provider(root: ByteString = completed.rootHash) = {
        val liveCpi = cpiUtxo.copy(output =
            cpiUtxo.output
                .asInstanceOf[TransactionOutput.Babbage]
                .copy(datumOption =
                    Some(DatumOption.Inline(CompletedPegInsMerkleTreeDatum(root).toData))
                )
        )
        val initial =
            (Seq(pir, liveCpi, configUtxo, bss) ++ refs).map(u => u.input -> u.output).toMap ++
                (60 to 62).map(n =>
                    input(n) -> TransactionOutput.Babbage(recipient, Value.ada(100))
                )
        Emulator.withRegisteredStakeCredentials(
          initial,
          Map(Credential.ScriptHash(pegIn.policyId) -> Coin.zero),
          Context(
            env = UtxoEnv(0L, chainInfo.protocolParams, CertState.empty, chainInfo.network),
            slotConfig = chainInfo.slotConfig
          )
        )
    }
    private val history = new CpoHistorySource {
        def backend = "offline"
        def addressHistory(address: String) = Right(
          if address == pegIn.address(Network.Testnet).encode.get then
              Seq(
                ChainOutput(
                  bytes(70, 32),
                  0,
                  Some(datum.copy(pegInUtxoId = priorId).toData),
                  Map((pegIn.policyId.toHex + bytes(71, 32).toHex) -> BigInt(1))
                )
              )
          else Seq.empty
        )
    }

    test(
      "preparation binds the recipient and proves the exact sweep value against the live roots"
    ) {
        val prepared = completion
            .prepare(provider(), history, snapshot, pir.input, recipient, timeout)
            .toOption
            .get
        assert(prepared.sweep.sweepingTmInput0 == sweptValue)
        assert(OnChainMPF(swept.rootHash).has(depositId, sweptValue, prepared.sweep.proof))
        assert(
          OnChainMPF(completed.rootHash)
              .insert(depositId, sweptValue, prepared.update.insertProof)
              .root == prepared.update.newRoot
        )
        val other = Address(Network.Testnet, Credential.KeyHash(AddrKeyHash.fromHex("ab" * 28)))
        val redirected = completion
            .prepare(provider(), history, snapshot, pir.input, other, timeout)
            .toOption
            .get
        assert(prepared.digest != redirected.digest)
        assert(prepared.signText != redirected.signText)
    }

    test(
      "real build preserves CPI and mints to the signed recipient while excluding every reference and explicit outpoint"
    ) {
        val chain = provider()
        val prepared =
            completion.prepare(chain, history, snapshot, pir.input, recipient, timeout).toOption.get
        val references = PegInCompletion.references(chain, pairs, timeout)
        val signature = ByteString.fromArray(
          wallet.identity
              .signMessage(
                ByteVector(prepared.signText.getBytes(java.nio.charset.StandardCharsets.UTF_8))
              )
              .toArray
        )
        val tx = completion
            .build(chain, sponsor, prepared, references, signature, Set(input(60)))
            .await(timeout)
        val body = tx.body.value
        val forbidden = refs.map(_.input).toSet + input(60)
        assert(body.inputs.toSet.intersect(forbidden).isEmpty)
        assert(body.collateralInputs.toSet.intersect(forbidden).isEmpty)
        assert(body.referenceInputs.toSet.contains(configUtxo.input))
        assert(body.referenceInputs.toSet.contains(bss.input))
        assert(
          body.mint.get.assets(token.policyId)(
            AssetName(ConfigDatum.BridgedTokenAssetName)
          ) == 30000
        )
        assert(body.mint.get.assets(pegIn.policyId)(AssetName(bytes(41, 32))) == -1)
        assert(
          body.outputs.exists(o =>
              o.value.address == recipient && o.value.value
                  .asset(token.policyId, AssetName(ConfigDatum.BridgedTokenAssetName)) == 30000
          )
        )
        val next = body.outputs.find(_.value.address == cpiUtxo.output.address).get.value
        assert(next.value == cpiUtxo.output.value)
        assert(
          next.inlineDatum.get.to[CompletedPegInsMerkleTreeDatum].root == prepared.update.newRoot
        )
        assert(chain.submit(tx).await(timeout).isRight)
    }

    test("unreachable live CPI root and incomplete manual prior list fail before building") {
        assert(
          completion
              .prepare(
                provider(MPF.empty.insert(id(99), id(98)).rootHash),
                history,
                snapshot,
                pir.input,
                recipient,
                timeout
              )
              .isLeft
        )
        assert(
          completion
              .prepare(provider(), history, snapshot, pir.input, recipient, timeout, Seq(depositId))
              .isLeft
        )
        assert(
          completion
              .prepare(provider(), history, snapshot, pir.input, recipient, timeout, Seq(priorId))
              .isRight
        )
    }

    test(
      "reference discovery verifies actual scripts and retains duplicate outpoints for exclusion"
    ) {
        val references = PegInCompletion.references(provider(), pairs, timeout)
        assert(references.excluded == refs.map(_.input).toSet)
        intercept[IllegalArgumentException] {
            PegInCompletion.references(provider(), Seq(pegIn.policyId -> refs(1).input), timeout)
        }
        intercept[IllegalArgumentException] {
            PegInCompletion.references(provider(), Seq(pegIn.policyId -> input(60)), timeout)
        }
    }

    test(
      "preparation rejects local script identities or swept tries that differ from the live snapshot"
    ) {
        val mismatched = snapshot.copy(context =
            snapshot.context.copy(config = config.copy(pegInScriptHash = bytes(99, 28)))
        )
        assert(
          completion.prepare(provider(), history, mismatched, pir.input, recipient, timeout).isLeft
        )
        assert(
          completion
              .prepare(
                provider(),
                history,
                snapshot.copy(trie = MPF.empty),
                pir.input,
                recipient,
                timeout
              )
              .isLeft
        )
    }

    test("a signature for another recipient is rejected by the real completion scripts") {
        val chain = provider()
        val prepared =
            completion.prepare(chain, history, snapshot, pir.input, recipient, timeout).toOption.get
        val signature = ByteString.fromArray(
          wallet.identity
              .signMessage(
                ByteVector(prepared.signText.getBytes(java.nio.charset.StandardCharsets.UTF_8))
              )
              .toArray
        )
        val other = Address(Network.Testnet, Credential.KeyHash(AddrKeyHash.fromHex("ab" * 28)))
        val redirected =
            completion.prepare(chain, history, snapshot, pir.input, other, timeout).toOption.get
        intercept[Exception] {
            completion
                .build(
                  chain,
                  sponsor,
                  redirected,
                  PegInCompletion.references(chain, pairs, timeout),
                  signature
                )
                .await(timeout)
        }
    }
}
