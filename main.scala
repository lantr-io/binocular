package binocular

import com.monovore.decline.*
import scalus.builtin.Builtins.*

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

