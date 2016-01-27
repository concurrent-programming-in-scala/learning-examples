package org.learningconcurrency
package exercises
package ch8

import akka.actor._
import akka.event.Logging
import akka.pattern._

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Try

import scala.util.{Failure, Success}


/**
 * Use actors to implement the ExecutionContext interface,
 * described in Chapter 3, Traditional Building Blocks of Concurrency.
 */
object Ex4 extends App {

  class ActorExecutionContext extends ExecutionContext {


    class ExecutorActor extends Actor() {

      val actorLog = Logging(context.system, this)

      override def receive: Receive = {
        case ExecutorActor.Execute(runnable) =>
          Try(runnable.run()) match {
            case Success(_) => actorLog.info("result OK")
            case Failure(e) => reportFailure(e)
          }
      }
    }

    object ExecutorActor {
      case class Execute(runnable: Runnable)
      def props = Props(new ExecutorActor)
    }

    val system = ActorSystem("MyActorSystem")
    val executeActor = system.actorOf(ExecutorActor.props)

    override def execute(runnable: Runnable): Unit = executeActor ! ExecutorActor.Execute(runnable)

    override def reportFailure(cause: Throwable): Unit = log(s"error: ${cause.getMessage}")

    def shutdown() = system.shutdown()
  }


  val executionContext = new ActorExecutionContext()

  executionContext.execute(new Runnable {
    override def run(): Unit = {
      log("run (exception)")
      throw new Exception("test exception")
    }
  })

  executionContext.execute(new Runnable {
    override def run(): Unit = {
      log("run")
    }
  })

  Thread.sleep(2000)

  executionContext.shutdown()


}
