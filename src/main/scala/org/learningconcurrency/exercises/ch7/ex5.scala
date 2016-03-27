package org.learningconcurrency
package exercises
package ch7

/**
  * Implement a transactional First In First Out (FIFO) queue,
  * represented with the TQueue class:
  *
  * class TQueue[T] {
  *    def enqueue(x: T)(implicit txn: InTxn): Unit = ???
  *    def dequeue()(implicit txn: InTxn): T = ???
  *  }
  *
  * The TQueue class has similar semantics as scala.collection.mutable. Queue,
  * but calling dequeue on an empty queue blocks until a value becomes available.
  */
object Ex5 extends App {

  import scala.collection.immutable.Queue
  import scala.concurrent.stm._
  import scala.concurrent.{ExecutionContext, Future}
  import ExecutionContext.Implicits.global

  class TQueue[T] {

    private val r = Ref[Queue[T]](Queue.empty[T])

    def enqueue(x: T)(implicit txn: InTxn): Unit = {
      r() = r() :+ x
    }

    def dequeue()(implicit txn: InTxn): T = {
      r().dequeueOption match {
        case None => retry
        case Some((x,q)) => {
          r() = q
          x
        }
      }
    }
  }

  //test
  val tQueue = new TQueue[Integer]

  val l = 1 to 20

  l.map { i =>
    Future {
      atomic {implicit txn =>
        val x = tQueue.dequeue

        Txn.afterCommit{_ =>
          log(s"dequeu: $x")
        }
      }
    }
  }

  l.map { i =>
    Future {
      atomic {implicit txn =>
        tQueue.enqueue(i)

        Txn.afterCommit { _ =>
          log(s"enque: $i")
        }
      }
    }
  }

  Thread.sleep(1000)
}
