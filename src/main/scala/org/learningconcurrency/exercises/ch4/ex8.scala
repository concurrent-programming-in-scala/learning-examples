package org.learningconcurrency
package exercises
package ch4

/**
 * Extend the Promise[T] type with the compose method,
 * which takes a function of the S => T type, and returns a Promise[S] object:
 *
 * def compose[S](f: S => T): Promise[S]
 *
 * Whenever the resulting promise is completed with some value x of the type S (or failed),
 * the original promise must be completed with the value f(x) asynchronously (or failed),
 * unless the original promise is already completed.
 *
 */

object Ex8 extends App {

  import scala.concurrent.{ExecutionContext, Future, Promise}
  import ExecutionContext.Implicits.global
  import scala.util.{Failure, Success}

  implicit class PromiseOps[T](val self: Promise[T]) {

    def compose[S](f: S => T): Promise[S] = {

      val ps = Promise[S]

      ps.future.onComplete {
        case Success(s) => Future(self.trySuccess(f(s)))
        case Failure(e) => self.tryFailure(e)
      }

      ps
    }
  }

  //test
  val pT = Promise[String]
  val pS: Promise[Int] = pT.compose((s) => s"val = $s")

  Future {
    Thread.sleep(1000)
    pS.success(1)
    //    pS.failure(new Exception)
  }



  pT.future foreach {
    case s => log(s)
  }

  pT.future.failed foreach { case t => log(s"q failed with $t") }


  Thread.sleep(2000)

}
