package org.learningconcurrency
package ch7






object RetryHeadWaitBad extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import CompositionSortedList._

  def headWait(lst: TSortedList): Int = atomic { implicit txn =>
    while (lst.head() == null) {}
    lst.head().elem
  }

  val myList = new TSortedList

  Future {
    val headElem = headWait(myList)
    log(s"The first element is $headElem")
  }
  Thread.sleep(1000)
  Future { myList.insert(1) }

}


object RetryHeadWait extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import CompositionSortedList._

  def headWait(lst: TSortedList): Int = atomic { implicit txn =>
    if (lst.head() != null) lst.head().elem
    else retry
  }

  val myList = new TSortedList

  Future {
    blocking {
      log(s"The first element is ${headWait(myList)}")
    }
  }
  Thread.sleep(1000)
  Future { myList.insert(1) }

}


object RetryChaining extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import CompositionSortedList._
  import RetryHeadWait._

  val list1 = new TSortedList
  val list2 = new TSortedList
  val allElements = Ref[List[Int]](Nil)

  def addToAll(x: Int) =
    allElements.single.transform(xs => x :: xs)

  val f = Future {
    atomic { implicit txn =>
      log(s"probe list1")
      addToAll(headWait(list1))
    } orAtomic { implicit txn =>
      log(s"probe list2")
      addToAll(headWait(list2))
    }
  }
  Future { list2.insert(2) }
  Thread.sleep(40)
  Future { list1.insert(1) }

  val g = for (_ <- f) yield allElements.single()

  g onSuccess {
    case elems => log(s"All elements: $elems")
  }

}


object RetryTimeouts extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._

  val message = Ref("")

  val f = Future {
    atomic.withRetryTimeout(1000) { implicit txn =>
      if (message() != "") s"got a message - ${message()}"
      else retry
    }
  }
  f onSuccess {
    case msg => log(msg)
  }

  Thread.sleep(1000)

  atomic { implicit txn =>
    message() = "Howdy!"
  }

}







