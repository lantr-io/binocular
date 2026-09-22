package binocular.cli.commands

import binocular.BinocularConfig
import binocular.bitcoin.Bip86Wallet
import binocular.cli.Command

/** Offline: no provider setup, RPC calls, worker startup or key files. */
final case class TrafficAddressCommand() extends Command {
    override def execute(config: BinocularConfig): Int = {
        val address = for {
            _ <- config.traffic.validate(config)
            wallet <- Bip86Wallet.fromMnemonic(
              config.wallet.mnemonic,
              config.bitcoinNode.bitcoinNetwork
            )
        } yield wallet.funding.address
        address match {
            case Right(value) => println(value); 0
            case Left(error)  => scala.Console.err.println(error); 1
        }
    }
}
