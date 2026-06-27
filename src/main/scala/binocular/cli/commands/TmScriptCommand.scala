package binocular.cli.commands

import binocular.*
import binocular.oracle.*
import binocular.watchtower.*
import binocular.cli.Command
import scalus.cardano.address.Address
import scalus.cardano.ledger.Credential
import scalus.uplc.builtin.ByteString

/** Export the compiled TreasuryMovementValidator so an external poster (heimdall `publish.rs`) can
  * mint the TM NFT under it and/or post to its address.
  *
  * Prints the policy id (= script hash = TM UTxO address credential), the bech32 address, and the
  * **single-CBOR-wrapped flat script** hex — the exact form heimdall's
  * `ProvidedScriptSource.script_cbor` expects (same encoding as `ALWAYS_OK_PLUTUS_CBOR_HEX`). The
  * script is parameterized by the oracle script hash + the TM-control NFT
  * (`bridge.tm-control-nft-{policy,name}`), so set those (from `deploy-bridge`) first — otherwise
  * this prints the placeholder (empty-param) script.
  */
case class TmScriptCommand() extends Command {

    override def execute(config: BinocularConfig): Int = {
        config.oracle.toBitcoinValidatorParams(config.bitcoinNode.bitcoinNetwork) match {
            case Left(err) =>
                System.err.println(s"Error deriving oracle params: $err")
                1
            case Right(params) =>
                val controlPolicy = config.bridge.tmControlNftPolicy
                val controlName = config.bridge.tmControlNftName
                if controlPolicy.isEmpty then
                    System.err.println(
                      "Warning: bridge.tm-control-nft-policy is unset — printing the placeholder " +
                          "(empty-param) script. Run deploy-bridge and set tm-control-nft-{policy,name} first."
                    )
                val oracleHash =
                    ByteString.fromArray(
                      BitcoinContract.script(params).scriptHash.bytes
                    )
                val contract = TreasuryMovementContract.script(
                  oracleHash,
                  ByteString.fromHex(controlPolicy),
                  ByteString.fromHex(controlName)
                )
                val policyId = contract.scriptHash
                val address =
                    Address(config.cardano.scalusNetwork, Credential.ScriptHash(policyId)).encode
                        .getOrElse("?")
                println(s"policy_id: ${policyId.toHex}")
                println(s"address:   $address")
                println(s"cbor:      ${contract.script.toHex}")
                0
        }
    }
}
