package binocular

import scalus.cardano.onchain.plutus.prelude.PairList.{PairCons, PairNil}
import scalus.cardano.onchain.plutus.prelude.{fail, List, PairList, SortedMap}
import scalus.cardano.onchain.plutus.v1.{PolicyId, TokenName}
import scalus.cardano.onchain.plutus.v3.Value
import scalus.compiler.Compile
import scalus.uplc.builtin.ByteString

import scala.annotation.tailrec

/** Optimized utilities
  */
@Compile
object StrictLookups {

    extension [A](self: List[A]) {
        @tailrec
        def findOrFail(predicate: A => Boolean): A = self match
            case List.Nil => fail("element not found")
            case List.Cons(head, tail) =>
                if predicate(head) then head else tail.findOrFail(predicate)

        def oneOrFail(message: String): A = self match
            case List.Cons(head, List.Nil) => head
            case _                         => fail(message)

    }

    extension [V](self: Value) {
        def existingQuantityOf(policyId: PolicyId, tokenName: TokenName): BigInt = {
            self.toSortedMap.lookupOrFail(policyId).lookupOrFail(tokenName)
        }
    }
    extension [V](self: SortedMap[ByteString, V]) {
        def lookupOrFail(key: ByteString): V = {
            @tailrec
            def go(lst: PairList[ByteString, V]): V = lst match
                case PairNil => fail("key not found")
                case PairCons((k, v), tail) =>
                    if key == k then v
                    else if key < k then fail("key not found")
                    else go(tail)

            go(self.toPairList)
        }
    }
}
