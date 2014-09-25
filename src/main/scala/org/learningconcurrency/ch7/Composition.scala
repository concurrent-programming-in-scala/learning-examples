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


object CompositionEscape extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._

  val myValue = Ref(0)

  atomic { implicit txn =>
    Future {
      Thread.sleep(500)
      myValue() = myValue() + 1
    } onComplete {
      case t => println(t)
    }
  }

  Thread.sleep(1000)
}


object CompositionCorrectSideEffect extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._

  val myValue = Ref(0)

  def inc() = atomic { implicit txn =>
    val valueAtStart = myValue()
    Txn.afterCommit { _ =>
      log(s"Incrementing $valueAtStart")
    }
    myValue() = myValue() + 1
  }

  Future { inc() }
  Future { inc() }

}


object CompositionLoggingRollback extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._

  val myValue = Ref(0)

  def inc() = atomic { implicit txn =>
    Txn.afterRollback { _ =>
      log(s"rollin' back")
    }
    myValue() = myValue() + 1
  }

  Future { inc() }
  Future { inc() }

}


object CompositionList extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._

  case class Node(val elem: Int, val next: Ref[Node]) {
    def append(n: Node): Unit = atomic { implicit txn =>
      val oldNext = next()
      next() = n
      n.next() = oldNext
    }
    def nextNode: Node = next.single()
    def appendIfEnd(n: Node) = next.single.transform {
      oldNext => if (oldNext == null) n else oldNext
    }
  }

  val nodes = Node(1, Ref(Node(4, Ref(Node(5, Ref(null))))))
  val f = Future { nodes.append(Node(2, Ref(null))) }
  val g = Future { nodes.append(Node(3, Ref(null))) }

  for (_ <- f; _ <- g) log(s"Next node is: ${nodes.nextNode}")

}


object CompositionMutations extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import CompositionList.Node

  def nodeToString(n: Node): String = atomic { implicit txn =>
    val b = new StringBuilder
    var curr = n
    while (curr != null) {
      b ++= s"${curr.elem}, "
      curr = curr.next()
    }
    b.toString
  }

}


object CompositionSortedList extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import CompositionList._
  import CompositionMutations._
  import scala.annotation.tailrec

  class TSortedList {
    val head = Ref[Node](null)

    override def toString = atomic { implicit txn =>
      val headNode = head()
      nodeToString(headNode)
    }

    def insert(x: Int): this.type = atomic { implicit txn =>
      @tailrec def insert(n: Node): Unit = {
        if (n.next() == null || n.next().elem > x) n.append(new Node(x, Ref(null)))
        else insert(n.next())
      }

      if (head() == null || head().elem > x) head() = new Node(x, Ref(head()))
      else insert(head())
      this
    }
  }

  val sortedList = new TSortedList

  val f = Future { sortedList.insert(1); sortedList.insert(4) }
  val g = Future { sortedList.insert(2); sortedList.insert(3) }

  for (_ <- f; _ <- g) log(s"sorted list - $sortedList")

}


object CompositionExceptions extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import CompositionSortedList._

  def pop(lst: TSortedList, n: Int): Unit = atomic { implicit txn =>
    var left = n
    while (left > 0) {
      lst.head() = lst.head().next()
      left -= 1
    }
  }

  val lst = new TSortedList
  lst.insert(4)
  lst.insert(9)
  lst.insert(1)
  lst.insert(16)

  Future { pop(lst, 2) } foreach { case _ => log(s"removed 2 elements - $lst") }
  Thread.sleep(1000)
  Future { pop(lst, 3) }.failed foreach { case t => log(s"oops $t - $lst") }
  Thread.sleep(1000)
  Future {
    atomic { implicit txn =>
      pop(lst, 1)
      sys.error("")
    }
  }.failed foreach { case t => log(s"oops again $t - $lst") }
  Thread.sleep(1000)

  import scala.util.control.Breaks._
  Future {
    breakable {
      atomic { implicit txn =>
        for (n <- List(1, 2, 3)) {
          pop(lst, n)
          break
        }
      }
    }
    log(s"after removing - $lst")
  }
  Thread.sleep(1000)

  import scala.util.control._
  Future {
    breakable {
      atomic.withControlFlowRecognizer {
        case c: ControlThrowable => false
      } { implicit txn =>
        for (n <- List(1, 2, 3)) {
          pop(lst, n)
          break
        }
      }
    }
    log(s"after removing - $lst")
  }

}


object CompositionCatchingExceptions extends App {
  import scala.concurrent.stm._
  import CompositionSortedList._
  import CompositionExceptions.pop

  val lst = new TSortedList
  lst.insert(4)
  lst.insert(9)
  lst.insert(1)
  lst.insert(16)

  atomic { implicit txn =>
    // note - a failed nested transaction executes the top-level `afterRollback` callback!
    // Txn.afterRollback { _ => log(s"afterRollback") }
    pop(lst, 2)
    log(s"lst = $lst")
    try { pop(lst, 3) }
    catch { case e: Exception => log(s"Houston... $e!") }
    pop(lst, 1)
  }

  log(s"result - $lst")
}





