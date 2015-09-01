package org.learningconcurrency
package exercises
package ch3

/**
 * Implement a LazyCell class with the following interface:
 *
 * class LazyCell[T](initialization: =>T) {
 * def apply(): T = ???
 * }
 *
 * Creating a LazyCell object and calling the apply method must have the
 * same semantics as declaring a lazy value and reading it, respectively.
 * You are not allowed to use lazy values in your implementation.
 *
 */

object Ex5 extends App {

  class LazyCellWithLazy[T](initialization: => T) {
    lazy val l = initialization
  }

  class LazyCell[T](initialization: => T) {

    @volatile
    var r: Option[T] = None

    def apply(): T = r match {
      case Some(v) => v
      case None => this synchronized {
        r match {
          case Some(v) => v
          case None => {
            r = Some(initialization)
            r.get
          }
        }
      }
    }
  }

  def func = {
    log("start...")
    Thread.sleep(10000)
    s"Calculation by ${Thread.currentThread().getName}"
  }

  val a = new LazyCell[String](func)

  import org.learningconcurrency.ch2._

  log("Start")

  val b = new LazyCellWithLazy[String](func)

  (0 to 50).
    map((i) => thread({
    Thread.sleep((Math.random * 10).toInt)
    println(a.apply)
  })).
    foreach(_.join)

}
