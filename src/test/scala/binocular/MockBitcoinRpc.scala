package binocular

import scala.concurrent.{ExecutionContext, Future}

/** Mock Bitcoin RPC that reads from test fixtures */
class MockBitcoinRpc(fixtureDir: String = "src/test/resources/bitcoin_blocks")(using
    ec: ExecutionContext
) extends BitcoinRpc {
    // Build hash→height index once to avoid scanning all files on every lookup
    private lazy val hashToHeight: Map[String, Int] = {
        val dir = new java.io.File(fixtureDir)
        dir.listFiles()
            .filter(_.getName.startsWith("block_"))
            .filter(_.getName.endsWith(".json"))
            .filterNot(_.getName.contains("merkle_proofs"))
            .map { f => BlockFixture.load(f) }
            .map(b => b.hash -> b.height)
            .toMap
    }

    private def loadBlockFixture(height: Int): BlockFixture =
        BlockFixture.load(height, fixtureDir)

    private def loadBlockByHash(hash: String): Option[BlockFixture] =
        hashToHeight.get(hash).map(loadBlockFixture)

    def getBlockHash(height: Int): Future[String] = Future {
        loadBlockFixture(height).hash
    }

    def getBlockHeader(hash: String): Future[BlockHeaderInfo] = Future {
        val fixture = loadBlockByHash(hash).getOrElse(
          throw new RuntimeException(s"Block not found: $hash")
        )
        if fixture.bits.isEmpty then
            throw new RuntimeException(
              s"Missing required field 'bits' for block ${fixture.height} (${fixture.hash})"
            )
        BlockHeaderInfo(
          hash = fixture.hash,
          height = fixture.height,
          version = fixture.version.toInt,
          merkleroot = fixture.merkleroot,
          time = fixture.timestamp,
          nonce = fixture.nonce,
          bits = fixture.bits,
          difficulty = 0.0,
          previousblockhash = fixture.previousblockhash
        )
    }

    def getBlock(hash: String): Future[BlockInfo] = Future {
        val fixture = loadBlockByHash(hash).getOrElse(
          throw new RuntimeException(s"Block not found: $hash")
        )
        if fixture.bits.isEmpty then
            throw new RuntimeException(
              s"Missing required field 'bits' for block ${fixture.height} (${fixture.hash})"
            )
        BlockInfo(
          hash = fixture.hash,
          height = fixture.height,
          version = fixture.version.toInt,
          merkleroot = fixture.merkleroot,
          time = fixture.timestamp,
          nonce = fixture.nonce,
          bits = fixture.bits,
          difficulty = 0.0,
          previousblockhash = fixture.previousblockhash,
          tx = fixture.transactions.map { txid =>
              TransactionInfo(txid = txid, hex = "")
          }
        )
    }

    def getRawTransaction(txid: String): Future[RawTransactionInfo] = Future {
        val blockHash = hashToHeight.keys
            .flatMap(h => loadBlockByHash(h))
            .find(_.transactions.contains(txid))
        blockHash match {
            case Some(block) =>
                RawTransactionInfo(
                  txid = txid,
                  hash = txid,
                  hex = "",
                  blockhash = Some(block.hash),
                  confirmations = 10
                )
            case None =>
                throw new RuntimeException(s"Transaction not found: $txid")
        }
    }

    def sendRawTransaction(hexString: String): Future[String] = Future {
        throw new RuntimeException("sendRawTransaction not supported in mock")
    }

    def getBlockchainInfo(): Future[BlockchainInfo] = Future {
        val maxHeight = hashToHeight.values.maxOption.getOrElse(0)
        val bestBlock = loadBlockFixture(maxHeight)
        BlockchainInfo(
          chain = "test",
          blocks = maxHeight,
          headers = maxHeight,
          bestblockhash = bestBlock.hash
        )
    }
}
