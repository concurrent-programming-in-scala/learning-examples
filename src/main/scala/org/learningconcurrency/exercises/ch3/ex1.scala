package org.learningconcurrency
package exercises
package ch3


/**
 * Implement a custom ExecutionContext class called PiggybackContext,
 * which executes Runnable objects on the same thread that calls execute.
 * Ensure that a Runnable object executing on the PiggybackContext
 * can also call execute and that exceptions are properly reported.
 */

import scala.util.{Failure, Success, Try}

object Ex1 extends App {

  import scala.concurrent._

  class PiggybackContext extends ExecutionContext {

    override def execute(runnable: Runnable): Unit = Try(runnable.run()) match {
      case Success(r) => log("result: OK")
      case Failure(e) => reportFailure(e)
    }

    override def reportFailure(cause: Throwable): Unit = {
      log(s"error: ${cause.getMessage}")
    }
  }

  val e = new PiggybackContext

  e.execute(new Runnable {
    override def run(): Unit = {
      log("run (exception)")
      throw new Exception("test exception")
    }
  })

  e.execute(new Runnable {
    override def run(): Unit = {
      log("run")
    }
  })




}
