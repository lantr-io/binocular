package binocular.cli.commands

import binocular.*
import binocular.cli.Command

/** Display oracle configuration and information */
case class InfoCommand() extends Command {

    override def execute(config: BinocularConfig): Int = {
        println("Binocular Bitcoin Oracle")
        println("=" * 60)

        val btc = config.bitcoinNode
        val cardano = config.cardano
        val oracle = config.oracle
        val wallet = config.wallet

        println()
        println("[Bitcoin Node]")
        println(s"  URL: ${if btc.url.nonEmpty then btc.url else "(not set)"}")
        println(s"  Network: ${btc.network}")

        println()
        println("[Cardano]")
        println(s"  Network: ${cardano.network}")
        println(s"  Backend: ${cardano.backend}")
        println(
          s"  Blockfrost Project ID: ${
                  if cardano.blockfrostProjectId.length > 8
                  then cardano.blockfrostProjectId.take(8) + "***"
                  else "(not set)"
              }"
        )

        println()
        println("[Oracle]")
        println(
          s"  TX Out Ref: ${if oracle.txOutRef.nonEmpty then oracle.txOutRef else "(not set)"}"
        )
        println(
          s"  Owner PKH: ${if oracle.ownerPkh.nonEmpty then oracle.ownerPkh else "(not set)"}"
        )
        oracle.toBitcoinValidatorParams() match {
            case Right(params) =>
                val addr = oracle.scriptAddress(cardano.cardanoNetwork)
                println(s"  Script Address: ${addr.getOrElse("(error)")}")
                println(s"  Maturation Confirmations: ${params.maturationConfirmations}")
                println(s"  Challenge Aging: ${params.challengeAging}")
                println(s"  Closure Timeout: ${params.closureTimeout}")
            case Left(_) =>
                println("  Script Address: (oracle not configured)")
        }
        println(s"  Start Height: ${oracle.startHeight.getOrElse("(not set)")}")
        println(s"  Max Headers/TX: ${oracle.maxHeadersPerTx}")
        println(s"  Poll Interval: ${oracle.pollInterval}s")
        println(s"  Retry Interval: ${oracle.retryInterval}s")
        println(s"  Transaction Timeout: ${oracle.transactionTimeout}s")

        println()
        println("[Wallet]")
        println(s"  $wallet")
        wallet.getAddress(cardano.scalusNetwork) match {
            case Right(addr) => println(s"  Address: $addr")
            case Left(_)     => println("  Address: (wallet not configured)")
        }

        0
    }
}
