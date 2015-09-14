package org.learningconcurrency
package exercises
package ch4

import scala.collection.immutable.IndexedSeq
import scala.concurrent._
import scala.util.{Failure, Success, Try}


object Ex6 extends App {

  /**
   * Implement the spawn method, which takes a command-line string,
   * asynchronously executes it as a child process,
   * and returns a future with the exit code of the child process:
   *
   * def spawn(command: String): Future[Int]
   *
   * Make sure that your implementation does not cause thread starvation.
   */

  import ExecutionContext.Implicits.global
  import scala.concurrent.{Future, Promise}
  import scala.sys.process._

  def spawn(command: String): Future[Int] = {

    val p = Promise[Int]

    Future {
      blocking {
        p.complete(Try(command !))
      }
    }

    p.future
  }


    val f = for (i <- 1 to 100) yield spawn("ping -c 10 google.com")

    f.foreach((p) => p.onComplete{
        case Success(i) => log(s"result = $i")
        case Failure(e) => log(s"Error !!!! ${e.toString}")
      }
    )

  Thread.sleep(10000)

}
