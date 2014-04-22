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

  case class Node(val elem: Int, val next: Ref[Node]) {
    def insertNext(n: Node): Unit = atomic { implicit txn =>
      val oldNext = next()
      next() = n
      n.next() = oldNext
    }
    def nextNode: Node = next.single()
  }

  val nodes = Node(1, Ref(Node(4, Ref(Node(5, Ref(null))))))
  val f = Future { nodes.insertNext(Node(2, Ref(null))) }
  val g = Future { nodes.insertNext(Node(3, Ref(null))) }

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


object CompositionNesting extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import CompositionList._
  import CompositionMutations._
  import scala.annotation.tailrec

  class ConcurrentSortedList {
    val head = Ref[Node](null)

    override def toString = atomic { implicit txn =>
      val headNode = head()
      nodeToString(headNode)
    }

    def insert(x: Int): Unit = atomic { implicit txn =>
      @tailrec def insert(n: Node) {
        if (n.next() == null || n.next().elem > x) n.insertNext(new Node(x, Ref(null)))
        else insert(n.next())
      }

      if (head() == null || head().elem > x) head() = new Node(x, Ref(head()))
      else insert(head())
    }
  }

  val sortedList = new ConcurrentSortedList

  val f = Future { sortedList.insert(1); sortedList.insert(4) }
  val g = Future { sortedList.insert(2); sortedList.insert(3) }

  for (_ <- f; _ <- g) log(s"sorted list - $sortedList")

}


object CompositionExceptions extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import CompositionNesting._

  def removeFirst(lst: ConcurrentSortedList, n: Int): Unit = atomic { implicit txn =>
    var left = n
    while (left > 0) {
      lst.head() = lst.head().next()
      left -= 1
    }
  }

  val lst = new ConcurrentSortedList
  lst.insert(4)
  lst.insert(9)
  lst.insert(1)
  lst.insert(16)

  Future { removeFirst(lst, 2) } onSuccess { case _ => log(s"removed 2 - $lst") }
  Thread.sleep(1000)
  Future { removeFirst(lst, 3) } onFailure { case t => log(s"oops $t - $lst") }
  Thread.sleep(1000)
  Future {
    atomic { implicit txn =>
      removeFirst(lst, 1)
      sys.error("")
    }
  } onFailure { case t => log(s"oops again $t - $lst") }
  Thread.sleep(1000)

  import scala.util.control.Breaks._
  Future {
    breakable {
      for (n <- List(1, 2, 3)) atomic { implicit txn =>
        removeFirst(lst, n)
        break
      }
    }
    log(s"after removing - $lst")
  }

}


object CompositionLoopsBad extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import CompositionNesting._

  def headWait(lst: ConcurrentSortedList): Int = atomic { implicit txn =>
    while (lst.head() == null) {}
    lst.head().elem
  }

  val myList = new ConcurrentSortedList

  Future {
    val headElem = headWait(myList)
    log(s"The first element is $headElem")
  }
  Thread.sleep(1000)
  Future { myList.insert(1) }

}











