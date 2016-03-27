package org.learningconcurrency
package exercises
package ch7

/**
  * Use ScalaSTM to implement the mutable location abstraction from Haskell,
  * represented with the MVar class:
  *
  * class MVar[T] {
  *   def put(x: T)(implicit txn: InTxn): Unit = ???
  *   def take()(implicit txn: InTxn): T = ???
  * }
  *
  * An MVar object can be either full or empty.
  * Calling put on a full MVar object blocks until the MVar object becomes empty,
  * and adds an element.
  *
  * Similarly, calling take on an empty MVar object blocks until the MVar object becomes full,
  * and removes the element.
  *
  * Now, implement a method called swap, which takes two MVar objects and swaps their values:
  *
  *  def swap[T](a: MVar[T], b: MVar[T])(implicit txn: InTxn) = ???
  *
  * Contrast the MVar class with the SyncVar class from Chapter 2,
  * Concurrency on the JVM and the Java Memory Model.
  * Is it possible to implement the swap method for SyncVar objects
  * without modifying the internal implementation of the SyncVar class?
  *
  */
object Ex2 extends App {

  import java.util.concurrent.atomic.AtomicInteger
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._

  class MVar[T] {

    private val rx = Ref[Option[T]](None)

    def put(x: T)(implicit txn: InTxn): Unit = rx() match {
      case Some(_) => retry
      case None => rx() = Some(x)
    }

    def take()(implicit txn: InTxn): T = rx() match {
      case Some(x) => {
        rx() = None
        x
      }
      case None => retry
    }
  }

  def swap[T](a: MVar[T], b: MVar[T])(implicit txn: InTxn) = {
    val old = a.take
    a.put(b.take())
    b.put(old)
  }

  //test
  val mVar = new MVar[Integer]

  val l = 1 to 1001

  l.map(
    i => Future {
      atomic {implicit txn =>
        mVar.put(i)
      }
    }
  )

  val sum = new AtomicInteger(0)

  l.map(
    i => Future {
      atomic {implicit txn =>
        val i = mVar.take
        Txn.afterCommit(_ => sum.addAndGet(i))
      }
    }
  )

  Thread.sleep(5000)

  if (l.sum != sum.get) log(s"Error !!!! ${l.sum} != $sum")

  log(s"summ = ${sum.get}")

  //test swap
  log("--- test swap ------------")

  val mva = new MVar[String]
  val mvb = new MVar[String]
  atomic {implicit txn =>
    mva.put("a")
    mvb.put("b")
  }

  l.map(i =>
    Future{
      atomic {implicit txn =>
        swap(mva, mvb)
      }
    }
  )

  Thread.sleep(5000)

  atomic {implicit txn =>
    val a = mva.take
    val b = mvb.take

    Txn.afterCommit( _ =>  log(s"a= $a, b = $b"))
  }
}
