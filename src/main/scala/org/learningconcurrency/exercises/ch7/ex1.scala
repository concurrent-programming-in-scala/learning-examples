package org.learningconcurrency
package exercises
package ch7

/**
  * Implement the transactional pair abstraction, represented with the TPair class:
  *
  *   class TPair[P, Q](pinit: P, qinit: Q) {
  *     def first(implicit txn: InTxn): P = ???
  *     def first_=(x: P)(implicit txn: InTxn): P = ???
  *     def second(implicit txn: InTxn): Q = ???
  *     def second_=(x: Q)(implicit txn: InTxn): Q = ???
  *     def swap()(implicit e: P =:= Q, txn: InTxn): Unit = ???
  *   }
  *
  * In addition to getters and setters for the two fields,
  * the transactional pair defines the swap method that swaps the  fields,
  * and can only be called if the types P and Q are the same.
  */
object Ex1 extends App {

  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._

  class TPair[P, Q](pinit: P, qinit: Q) {

    private val rFirst = Ref[P](pinit)
    private val rSecond = Ref[Q](qinit)


    def first(implicit txn: InTxn): P = rFirst.single()

    def first_=(x: P)(implicit txn: InTxn) = rFirst.single.transform(old => x)

    def second(implicit txn: InTxn): Q = rSecond.single()

    def second_=(x: Q)(implicit txn: InTxn) = rSecond.single.transform(old => x)

    def swap()(implicit e: P =:= Q, txn: InTxn): Unit = {
      val old = first
      first = second.asInstanceOf[P]
      second = e(old)
    }
  }

  //test
  val p = new TPair[String,String]("first value","second value")

  def swapOne = atomic { implicit txn =>
    p.swap

    val vF = p.first
    val vS = p.second

    Txn.afterCommit { _ =>
      assert(vS != vF)
    }
  }

  (1 to 1001).map(_ => Future {
    swapOne
  })

  Thread.sleep(2000)

  atomic {implicit txn =>
    log(s"Result: first = '${p.first}' vSecond = '${p.second}'")
  }


}