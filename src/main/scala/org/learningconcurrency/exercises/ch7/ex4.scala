package org.learningconcurrency
package exercises
package ch7

/**
  * Implement the atomicWithRetryMax method,
  * which is used to start a transaction that can be retried at most n times:
  *
  * def atomicWithRetryMax[T](n: Int)(block: InTxn => T): T = ???
  *
  * Reaching the maximum number of retries throws an exception.
  */
object Ex4 extends App {

  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import scala.util.Random

  case class ReachMaxNumberException(cntRetries: Int) extends Exception

  def atomicWithRetryMax[T](n: Int)(block: InTxn => T): T = {

    var cntRetries = 0

    atomic{ implicit txn =>
      Txn.afterRollback(_ => cntRetries += 1)

      if (cntRetries > n) {
        throw  ReachMaxNumberException(cntRetries)
      }

      block(txn)
    }
  }

  //test
  val r = Ref(10)

  def block(txn: InTxn): Int = {
    var x: Int = r.get(txn)
    Thread.sleep(10)
    x += Random.nextInt(100)
    r.set(x)(txn)
    x
  }

  (1 to 100).map(i =>
    Future {
      try {
        atomicWithRetryMax[Int](3)(block)
        log(s"Transaction: $i - ok")
      } catch {
        case ReachMaxNumberException(cntRetries) => log(s"Transaction: $i (retries = $cntRetries)")
      }
    }
  )

  Thread.sleep(3000)
}
