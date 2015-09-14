package org.learningconcurrency
package exercises
package ch3

import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec

/**
 * Implement a PureLazyCell class with the same interface and semantics as the LazyCell class from the previous exercise.
 * The PureLazyCell class assumes that the initialization parameter does not cause side effects,
 * so it can be evaluated more than once.
 * The apply method must be lock-free and should call the initialization as little as possible.
 */

object Ex6 extends App {

  class PureLazyCell[T](initialization: => T) {

    val r = new AtomicReference[Option[T]](None)

    @tailrec
    final def apply(): T = r.get match {
      case Some(v) => v
      case None => {
        val v = initialization
        if (!r.compareAndSet(None, Some(v))) apply()
        else v
      }
    }
  }

  def initialization = {
    log("calculation ...")
    Thread.sleep(1000)
    s"result (calculate by ${Thread.currentThread().getName})"
  }

  val p = new PureLazyCell[String](initialization)

  import org.learningconcurrency.ch2.thread

  log("start")

  val t = (1 to 10).map((i) => thread {
    val sleep = (Math.random * 10000).toInt
    Thread.sleep(sleep)

    (1 to 3).foreach((i) => log(s"v$i = ${p.apply}"))
  })

  t.foreach(_.join)

}
