package org.learningconcurrency
package exercises
package ch7

/**
  * Implement the atomicRollbackCount method, which is used to track how many times
  * a transaction was rolled back before it completed successfully:
  *
  *   def atomicRollbackCount[T](block: InTxn => T): (T, Int) = ???
  */
object Ex3 extends App {

  import scala.concurrent.ExecutionContext
  import scala.concurrent.Future
  import scala.concurrent.stm._
  import ExecutionContext.Implicits.global
  import scala.util.Random

  def atomicRollbackCount[T](block: InTxn => T): (T, Int) = {
    var cnt = 0
    atomic { implicit txn =>
      Txn.afterRollback(_ =>
        cnt += 1
      )
      (block(txn), cnt)
    }
  }

  //test
  val r = Ref(10)

  def block(txn: InTxn): Int = {
    var x: Int = r.get(txn)
    x = Random.nextInt(10000)
    Thread.sleep(10)
    r.set(x)(txn)
    x
  }

  (1 to 100).map(i =>
    Future {
      atomicRollbackCount[Int](block) match {
        case (_, cnt) => log(s"Transaction: $i, retries = $cnt")
        case _ => log("???")
      }
    }
  )

  Thread.sleep(3000)
}
