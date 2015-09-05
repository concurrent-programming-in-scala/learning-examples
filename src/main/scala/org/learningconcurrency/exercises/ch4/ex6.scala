package org.learningconcurrency
package exercises
package ch4

import scala.concurrent.ExecutionContext
import scala.util.Try


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
      p.complete(Try(command !))
    }

    p.future
  }


  val f = spawn("ls -r")

  f foreach {
    case i => log(s"result = $i")
  }

  f.failed foreach {
    case e => log(s"Error !!!! ${e.toString}")
  }


  Thread.sleep(2000)

}
