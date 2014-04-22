package org.learningconcurrency
package ch7






object CompositionSideEffects extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._

  val myValue = Ref(0)

  def inc() = atomic { implicit txn =>
    log(s"Incrementing ${myValue()}")
    myValue() = myValue() + 1
  }

  Future { inc() }
  Future { inc() }

}


object CompositionList extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._

  class Node(val elem: Int, val next: Ref[Node]) {
    def insertAfter(n: Node): Unit = atomic { implicit txn =>
      val oldNext = next()
      next() = n
      n.next() = oldNext()
    }
  }



}


object CompositionNesting extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import CompositionList._

  class ConcurrentSortedList {
  }
}
