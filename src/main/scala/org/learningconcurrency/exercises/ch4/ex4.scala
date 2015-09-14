package org.learningconcurrency
package exercises
package ch4


/**
 * Repeat the previous exercise, but use Promise objects instead of future combinators.
 */

object Ex4 extends App {

  import scala.concurrent.{ExecutionContext, Future, Promise}
  import ExecutionContext.Implicits.global

  implicit class FutureOps[T](val self: Future[T]) {

    def exists(p: T => Boolean): Future[Boolean] = {

      val pr = Promise[Boolean]
      self foreach ((t: T) => pr.success(p(t)))
      self.failed foreach (_ => pr.success(false))
      pr.future
    }
  }

  //test

  import scala.concurrent.duration.Duration

  val f1 = Future {
    100
  }
  val f2 = Future {
    -100
  }
  val f3 = Future {
    throw new Exception("Error")
  }

  def p(i: Int) = i > 0

  import scala.concurrent.Await

  log("f1 = " + Await.result(f1.exists(p), Duration.Inf).toString)
  log("f2 = " + Await.result(f2.exists(p), Duration.Inf).toString)
  log("f3 = " + Await.result(f3.exists(p), Duration.Inf).toString)

}
