package org.learningconcurrency
package exercises
package ch8

import akka.actor._
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

  import org.learningconcurrency.exercises.ch8.Ex4.ExecutorActor.Execute

  class ActorExecutionContext extends ExecutionContext {

    val system = ActorSystem("MyActorSystem")
    val actor = system.actorOf(Props[ExecutorActor], "executorActor")

    override def execute(runnable: Runnable): Unit = {
      (actor ? Execute(runnable))(5 seconds).mapTo[Try[Unit]] onComplete {
        case Success(r) => log(s"result OK")
        case Failure(e) => reportFailure(e)
      }
    }

    override def reportFailure(cause: Throwable): Unit = {
      log(s"error: ${cause.getMessage}")
    }

  }

  class ExecutorActor extends Actor {
    override def receive: Receive = {
      case Execute(runnable) => sender ! Try(runnable.run())
    }
  }

  object ExecutorActor {
    case class Execute(runnable: Runnable)
  }

  val e = new ActorExecutionContext

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
