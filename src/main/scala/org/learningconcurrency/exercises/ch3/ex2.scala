package org.learningconcurrency
package exercises
package ch3

/**
 * Implement a TreiberStack class, which implements a concurrent stack abstraction:
 *  class TreiberStack[T] {
 *    def push(x: T): Unit = ???
 *       def pop(): T = ???
 *  }
 * Use an atomic reference variable that points to a linked list of nodes that were previously pushed to the stack.
 * Make sure that your implementation is lock-free and not susceptible to the ABA problem.
 */

import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec

object Ex2 extends App {

  class TreiberStack[T] {

    var r = new AtomicReference[List[T]](List.empty[T])

    @tailrec
    final def push(x: T): Unit = {
      val oldList = r.get
      val newList = x::oldList
      if (!r.compareAndSet(oldList,newList)) push(x)
    }

    @tailrec
    final def pop(): T = {
      val oldList = r.get
      val newList = oldList.tail
      if (r.compareAndSet(oldList,newList)) oldList.head
      else pop()
    }

  }

  import org.learningconcurrency.ch2._

  val s = new TreiberStack[Int]

  val t1 = thread {
    for (i <- 1 to 10) {
      s.push(i)
      Thread.sleep(1)
    }
  }

  val t2 = thread  {
    for (i <- 1 to 10) {
      s.push(i*10)
      Thread.sleep(1)
    }
  }

  t1.join()
  t2.join()

  for (i <- 1 to 20)
    log(s"s[$i] = ${s.pop()}")


}
