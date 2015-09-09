package org.learningconcurrency
package exercises
package ch4

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import scala.util.Try

/**
 * Implement an abstraction called a single-assignment variable, represented by the IVar class:
 *
 * class IVar[T] {
 * def apply(): T = ???
 * def :=(x: T): Unit = ???
 * }
 *
 * When created, the IVar class does not contain a value, and calling apply results in an exception.
 * After a value is assigned using the := method, subsequent calls to := throw an exception,
 * and the apply method returns the previously assigned value.
 *
 * Use only futures and promises, and avoid the synchronization primitives from the previous chapters.
 *
 */

object Ex2 extends App {

  class IVar[T] {

    val p = Promise[T]

    def apply(): T =
      if (p.isCompleted) Await.result(p.future, Duration.Inf)
      else throw new Exception("Not contain a value")

    def :=(x: T): Unit = if (!p.tryComplete(Try(x))) throw new Exception("Value is already assigned")
  }

  import org.learningconcurrency.ch2.thread

  val v = new IVar[String]
  (1 to 10).foreach((i) => thread {
    try {
      v := s"v = ${Thread.currentThread().getName}"
    } catch {
      case e:Throwable => log(s"Error !!! ${e.getMessage}. Current value = ${v.apply}")
    }
  }
  )

}
