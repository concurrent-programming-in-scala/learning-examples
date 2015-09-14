package org.learningconcurrency
package exercises
package ch4

/**
 * Extend the Future[T] type with the exists method, which takes a predicate and returns a Future[Boolean] object:
 *
 * def exists(p: T => Boolean): Future[Boolean]
 *
 * The resulting future is completed with true if and only if the original future is completed
 * and the predicate returns true, and false otherwise.
 * You can use future combinators, but you are not allowed to create any Promise objects in the implementation.
 *
 * The existing Future combinators to actually fail the resulting future when the predicate p throws an exception
 */

object Ex3 extends App {

  import scala.concurrent.{ExecutionContext, Future}
  import ExecutionContext.Implicits.global

  implicit class FutureOps[T](val self: Future[T]) {
    def exists(p: T => Boolean): Future[Boolean] = self.map(p)
  }

  //test
  val f1 = Future {
    100
  }
  val f2 = Future {
    -100
  }
  val f3 = Future {
    throw new Exception("Error in predicate")
  }

  def p(i: Int) = i > 0

  import scala.concurrent.Await
  import scala.concurrent.duration.Duration

  log("f1 = " + Await.result(f1.exists(p), Duration.Inf).toString)
  log("f2 = " + Await.result(f2.exists(p), Duration.Inf).toString)

  f3.failed foreach {
    case t:Throwable => log(t.getMessage)
  }

  //log("f3 = " + Await.result(f3.exists(p), Duration(5,"sec")).toString)
}
