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

  val queue1 = new TSortedList
  val queue2 = new TSortedList

  val consumer = Future {
    blocking {
      atomic { implicit txn =>
        log(s"probe queue1")
        log(s"got: ${headWait(queue1)}")
      } orAtomic { implicit txn =>
        log(s"probe list2")
        log(s"got: ${headWait(queue2)}")
      }
    }
  }
  Thread.sleep(50)
  Future { queue2.insert(2) }
  Thread.sleep(50)
  Future { queue1.insert(1) }

}


object RetryTimeouts extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._

  val message = Ref("")

  Future {
    blocking {
      atomic.withRetryTimeout(1000) { implicit txn =>
        if (message() != "") s"got a message - ${message()}"
        else retry
      }
    }
  } foreach {
    case msg => log(msg)
  }

  Thread.sleep(1025)

  atomic { implicit txn =>
    message() = "Howdy!"
  }

}


object RetryFor extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._

  val message = Ref("")

  Future {
    blocking {
      atomic { implicit txn =>
        if (message() == "") {
          retryFor(1000)
          log(s"no message - '${message()}'")
        } else log(s"got a message - '${message()}'")
      }
    }
  }

  Thread.sleep(1025)

  message.single() = "Howdy!"

}






