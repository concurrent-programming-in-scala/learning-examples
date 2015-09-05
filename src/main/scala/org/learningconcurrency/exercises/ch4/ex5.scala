package org.learningconcurrency
package exercises
package ch4


/**
 * Repeat the previous exercise, but use the Scala Async framework.
 */

object Ex5 extends App {

  import scala.async.Async.{async, await}
  import scala.concurrent.{ExecutionContext, Future}
  import ExecutionContext.Implicits.global

  implicit class FutureOps[T](val self: Future[T]) {
    def exists(p: T => Boolean): Future[Boolean] =
      async {
        val v = await {
          self
        }
        p(v)
      }.recover { case _ => false }
  }

  //test
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
  import scala.concurrent.duration.Duration

  log("f1 = " + Await.result(f1.exists(p), Duration.Inf).toString)
  log("f2 = " + Await.result(f2.exists(p), Duration.Inf).toString)
  log("f3 = " + Await.result(f3.exists(p), Duration.Inf).toString)


}
