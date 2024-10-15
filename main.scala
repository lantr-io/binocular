package binocular

import binocular.BitcoinValidator.BlockHeader
import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.address.AddressProvider
import com.bloxbean.cardano.client.backend.api.BackendService
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.common.model.Network
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.crypto.cip1852.CIP1852
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath
import com.bloxbean.cardano.client.plutus.spec.*
import com.monovore.decline.*
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.PlatformSpecific
import scalus.sir.RemoveRecursivity
import scalus.uplc.Program
import scalus.uplc.Term

def info(): Unit =
    println(s"Script address")

enum Cmd:
    case Info

val command =
    val infoCommand = Opts.subcommand("info", "Prints the contract info") {
        Opts(Cmd.Info)
    }

    Command(name = "binocular", header = "Binocular")(
      infoCommand
    )

@main def main(args: String*): Unit =
    command.parse(args) match
        case Left(help) => println(help)
        case Right(cmd) =>
            cmd match
                case Cmd.Info => info()

