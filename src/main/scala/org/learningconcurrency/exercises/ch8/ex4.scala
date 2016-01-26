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

  class ExecutorActor extends Actor with ExecutionContext {

    val log = Logging(context.system, this)

    override def receive: Receive = {
      case ExecutorActor.Execute(runnable) =>
        Try(runnable.run()) match {
          case Success(_) => log.info("result OK")
          case Failure(e) => reportFailure(e)
        }
    }

    override def execute(runnable: Runnable): Unit = self ! ExecutorActor.Execute(runnable)

    override def reportFailure(cause: Throwable): Unit = {
      log.error(s"error: ${cause.getMessage}")
    }

  }

  object ExecutorActor {
    case class Execute(runnable: Runnable)
  }

  val system = ActorSystem("MyActorSystem")
  val executeActor = system.actorOf(Props[ExecutorActor], "executorActor")

  executeActor ! ExecutorActor.Execute(new Runnable {
    override def run(): Unit = {
      log("run (exception)")
      throw new Exception("test exception")
    }
  })

  executeActor ! ExecutorActor.Execute(new Runnable {
    override def run(): Unit = {
      log("run")
    }
  })

  Thread.sleep(2000)

  system.shutdown()


}
