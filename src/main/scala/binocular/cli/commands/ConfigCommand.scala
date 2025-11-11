package binocular.cli.commands

import binocular.{BitcoinNodeConfig, CardanoConfig, OracleConfig, WalletConfig}
import binocular.cli.Command

/** Show complete configuration (all subsystems) */
case class ConfigCommand() extends Command {

    override def execute(): Int = {
        println("Binocular Configuration")
        println("=" * 60)

        // Load all configs
        val bitcoinConfig = BitcoinNodeConfig.load()
        val cardanoConfig = CardanoConfig.load()
        val oracleConfig = OracleConfig.load()
        val walletConfig = WalletConfig.load()

        println("\n[Bitcoin Node]")
        bitcoinConfig match {
            case Right(config) => println(config)
            case Left(err) => println(s"Error: $err")
        }

        println("\n[Cardano Backend]")
        cardanoConfig match {
            case Right(config) => println(config)
            case Left(err) => println(s"Error: $err")
        }

        println("\n[Oracle]")
        oracleConfig match {
            case Right(config) => println(config)
            case Left(err) => println(s"Error: $err")
        }

        println("\n[Wallet]")
        walletConfig match {
            case Right(config) =>
                println(config)
                config.getAddress() match {
                    case Right(addr) => println(s"Address: $addr")
                    case Left(err) => println(s"Error getting address: $err")
                }
            case Left(err) => println(s"Error: $err")
        }

        // Return error if any config failed
        val hasError = List(bitcoinConfig, cardanoConfig, oracleConfig, walletConfig).exists(_.isLeft)
        if (hasError) 1 else 0
    }
}
